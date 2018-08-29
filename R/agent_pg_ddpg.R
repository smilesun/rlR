#' @title Deep Deterministic Policy Gradient
#'
#' @format \code{\link{R6Class}} object
#' @description Continous action agent
#' Inherited from \code{AgentActorCritic}:
#' @section Methods:
#' @inheritSection AgentArmed Methods
#' reference Lillicrap, T. P., Hunt, J. J., Pritzel, A., Heess, N., Erez, T., Tassa, Y., … Wierstra, D. (2016). Continuous control with deep reinforcement learning. In ICLR.
#' @return [\code{\link{AgentDDPG}}].
#' @export
AgentDDPG = R6::R6Class("AgentDDPG",
  inherit = AgentActorCritic,
  public = list(
    tau = NULL,  # bilinear combination of target and update network
    optimize = NULL,
    grad2a = NULL,
    ph_critic2act = NULL,
    actor_pred = NULL,
    model = NULL,
    list.states.next = NULL,
    list.states.old = NULL,
    input_action_update = NULL,
    input_state_update = NULL,
    input_state_actor_update  = NULL,
    input_actor_update_weights = NULL,
    brain_actor_update = NULL,
    brain_critic_update = NULL,
    brain_actor_target = NULL,
    brain_critic_target = NULL,
    replay_actions = NULL,
    np = NULL,
    tb.acts = NULL,  # acts from replay memory
    tb_acts_target_policy = NULL,  # acts according to policy with respect to states
    tb_acts_update_policy = NULL,  # acts according to policy with respect to states
    tb.state = NULL,
    tb_state_new = NULL,
    tb.targets = NULL,
    initialize = function(env, conf) {
      self$np = reticulate::import("numpy", convert = FALSE)
      self$tau = 0.1
      super$initialize(env, conf)
      self$setBrain()
    },

    createBrain = function() {
      if (self$task == "value_fun") {
        tuple = createCriticNetwork(state_dim = self$state_dim, action_dim = 1L)
        self$input_action_update = tuple$input_action
        self$input_state_update = tuple$input_state
        return(tuple$model)
      } else if (self$task == "policy_fun"){
        tuple = createActorNetwork(state_dim = self$state_dim, action_dim = 1L)
        self$input_state_actor_update = tuple$input_state
        self$input_actor_update_weights = tuple$weights
        return(tuple$model)
      }
    },

    customizeBrain = function() {
      stop("not supported!")
    },

    setBrain = function() {
      self$task = "value_fun"
      self$brain_critic_target = SurroNN$new(self)
      self$brain_critic_update = SurroNN$new(self)
      self$task = "policy_fun"
      self$brain_actor_target = SurroNN$new(self)
      self$brain_actor_update = SurroNN$new(self)
      self$model = self$brain_critic_update
      self$trainActorSessInit()
      self$sess$run(tf$initialize_all_variables())
    },

    # target: r_i + gamma Q_{target}(s_new, policy_action)
    # policy_action is generated from the target policy network
    extractCriticTarget = function(i) {
      y = self$list.rewards[[i]] + self$gamma * self$p.next[i, ]
      return(as.array(y))
    },

    # input: (state, action)
    # output: state-action value
    trainCritic = function() {
      self$getYhat()
      self$tb.acts = Reduce(rbind, self$list.acts)
      self$tb.state = Reduce(rbind, self$list.states.old)
      len = length(self$list.replay)
      list.targets = lapply(1:len, self$extractCriticTarget)
      self$tb.targets = Reduce(rbind, list.targets)
      # yhat of critic is Q_{update}(s_i, a_i)
      if (dim(self$tb.targets)[1L] != 1) {
  	self$fitUpdateCriticNetwork(action_input = self$tb.acts, state_input = self$tb.state, yhat = self$tb.targets)
      }
    },

    # actor is also with loss mse since action is continous!!
    extractActorTarget = function(i) {
      act = self$list.acts[[i]]
      #FIXME: how to find a target here?
      target = self$advantage[i, ] + self$actor_pred[i, ]
      return(target)
    },

    trainActorSessInit = function(state_input, input_criticQ2act) {
      self$ph_critic2act = tf$placeholder(dtype = tf$float32, shape = shape(NULL, self$act_cnt), name = "criticQ2a")  # place holder for action
      # chain rule: set initial value of the gradient to be -ph_critic2act
      tensor_grad_policy2theta = tf$gradients(ys = self$brain_actor_update$model$output, xs = self$brain_actor_update$model$weights, grad_ys = tf$negative(self$ph_critic2act))
      # The final gradients are Q(s_t,a = \mu(s_t)) with respect to \theta^{\mu}(actor network weights), the graph is \theta^{mu}(weights of actor network) - action(a = \mu(s)) - Q(s, a)
      # grad is gradient, vars are the variable to be applied the gradients
      grad_and_vars = reticulate::tuple(tensor_grad_policy2theta, self$brain_actor_target$model$weights)
      grad_and_vars = mapply(reticulate::tuple, tensor_grad_policy2theta, self$brain_actor_target$model$weights)
      #x <- 1:3
      #y <- 4:6
      #mapply(list, x, y, SIMPLIFY = F) # gives a list of 3 tuples res[[3]][[1L]] = 3, res[[3]][[2L]] = 6
      #mapply(c, x, y, SIMPLIFY=F) # gives a list of 3 tuples, res[[3]] = c(3, 6)
      opt = tf$train$AdamOptimizer(0.001)
      self$optimize = opt$apply_gradients(grad_and_vars)
    },

    trainActorSess = function(state_input, input_criticQ2act) {
      sname = self$brain_actor_update$model$input$name
      aname = self$ph_critic2act$name
      sstate = self$np$array(state_input)
      scritic2act = self$np$array(input_criticQ2act)
      feed_dict = py_dict(c(sname, aname), c(sstate, scritic2act))
      self$sess$run(self$optimize, feed_dict = feed_dict)
    },

    trainActorPrepare = function() {
      # $a = \mu(s_i)$
      self$tb_acts_update_policy = self$brain_actor_update$pred(self$tb.state)
      # $\nabla_aQ(s_i, a = \mu(s_i))$
      self$grad2a = self$brain_critic_update$calGradients2Action(state_input = self$tb.state, action_input = self$tb_acts_update_policy)
      #FIXME: why the return from tensor flow is a list?
      self$grad2a = self$grad2a[[1L]]
      #FIXME: grad2a should be a scalar!
      #NOTE: grad2a is $\nabla_a Q(s_i, a = \mu(s_i))$ where $\mu(s_i)$ is the policy network
      # s ->[policy \mu_{\theta}(s)] a | (a, s) ->[value] Q(w,a,s)
      # w in Q(w, a, s) is updated via bellman equation  with fixed (a:a_i, s:s_i)
      # $\theta$  in $\mu_{\theta}(s)$ is updated in a way to maximize the Q(s_i, a=\mu(s_i))
      # gradient for $\theta$ is $\nabla_{\theta} Q(s_i, a = \mu(s_i)) = $\nabla_{a} Q(s_i, a = \mu(s_i)) %*% \nabla_{a} Q(s_i, a = \mu(s_i))$ which is matrix multiplication for chain rule, so element wise mulitiplication for usuall policy network does not work here.

      #self$actor_pred =  self$brain_actor_update$pred(self$tb.state)
      #self$advantage = self$grad2a * self$actor_pred
      #len = dim(self$replay.x)[1L]
      #list.targets = lapply(1:len, self$extractActorTarget)
      #tb.targets = Reduce(rbind, list.targets)
      #self$fitActor(self$tb.state, tb.targets)
    },

    replay = function(size) {
      self$unpack(size)
      self$trainCritic()
      if (dim(self$tb.targets)[1L] != 1) {
        self$trainActorPrepare()
        self$trainActorSess(self$tb.state, self$grad2a)
        self$updateModel()
      }
    },

    # Ornstein–Uhlenbeck process: c(Gaussian Process, Markov Process, Temporirily Homogeneous)
    # random walk in continous time (Wiener Process) Continous time AR(1)
    # Over time, the process tends to drift towards its long-term mean: such a process is called mean-reverting. (Going back and forth around the mean). properties of the process have been changed so that there is a tendency of the walk to move back towards a central location, with a greater attraction when the process is further away from the center.
    # $dx_t = \theta(\mu - x_t)dt + \sigma dWt$ where $W_t$ is the Wiener Process
    # the probability density follows the Fokker-Planck equation with the infinite time solution $f(x) = \sqrt(\theta / (\pi \sigma^2))e^{-\theta(x-\mu)^2/\sigma^2}$
    # stationary distribution is gaussian with var(x) = \sigma^2/(2 * theta)
    ou = function(act) {
      mu = 0  # going back and forth around 0
      theta = 0.60  # exponential shoulder factor
      sigma = 0.30
      # one step differential equation change, since action_new = action_old + ou(action_old)
      # so d(action) = action_new - action_old = ou(action_old) = \theta(\mu - x_t)dt + \sigma dWt
      # where the difference of Wiener process dWt is white noise
      theta * (mu - act) + sigma * rnorm(1)
    },

    evaluateArm = function(state) {
      act_cc_nn = self$brain_actor_update$pred(state)
      self$vec.arm.q = act_cc_nn + self$policy$epsilon * self$ou(act_cc_nn)  # continous action
    },

    act = function(state) {
      checkmate::assert_array(state)
      state = array_reshape(state, c(1, self$state_dim))
      self$evaluateArm(state)
      return(self$vec.arm.q)  # gym need an array as action
    },

    updateModel = function() {
      # actor
      uaw = self$brain_actor_update$getWeights()
      uaw = lapply(uaw, function(x) x * self$tau)
      taw = self$brain_actor_target$getWeights()
      taw = lapply(taw, function(x) x * (1.0 - self$tau))
      www = mapply("+", uaw, taw)
      self$brain_actor_target$setWeights(www)
      # critic
      uaw = self$brain_critic_update$getWeights()
      uaw = lapply(uaw, function(x) x * self$tau)
      taw = self$brain_critic_target$getWeights()
      taw = lapply(taw, function(x) x * (1.0 - self$tau))
      www = mapply("+", uaw, taw)
      self$brain_critic_target$setWeights(www)
    },

    predTargetCritic = function(action_input, state_input) {
      #FIXME: the fixed order of action_input and state_input might be problematic
      res = keras::predict_on_batch(self$brain_critic_target$model, x = list(action_input, state_input))
      return(res)
    },

    fitUpdateCriticNetwork = function(action_input, state_input, yhat) {
      #FIXME: the fixed order of action_input and state_input might be problematic
      res = keras::fit(self$brain_critic_update$model, x = list(action_input, state_input), y = yhat, epochs = 1L, verbose = FALSE)
      return(res)
    },

    getYhat = function(...) {
      self$tb_state_new = Reduce(rbind, self$list.states.next)
      self$tb_acts_target_policy = self$brain_actor_target$pred(self$tb_state_new)
      self$p.next = self$predTargetCritic(self$tb_acts_target_policy, self$tb_state_new)
    },

    unpack = function(batchsize) {
      self$list.replay = self$mem$sample.fun(batchsize)
      self$list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
      self$list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
      self$list.rewards = lapply(self$list.replay, ReplayMem$extractReward)
      self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
      temp = simplify2array(self$list.states.old) # R array put elements columnwise
      mdim = dim(temp)
      norder = length(mdim)
      self$replay.x = aperm(temp, c(norder, 1:(norder - 1)))
    },

    afterStep = function() {
      self$policy$afterStep()
    },

    afterEpisode = function(interact) {
      self$replay(self$replay.size)
      self$policy$afterEpisode()
      self$mem$afterEpisode()
    }

))


# normal 1 arm output network with only state as input
createActorNetwork = function(state_dim = 3, action_dim = 1L) {
  input_state = keras::layer_input(shape = state_dim)
  states_hidden = input_state %>%
    layer_dense(units = 27, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = 27, activation = "linear") %>%
    layer_dense(units = action_dim, activation = "linear")  # only 1L output!
  model = keras::keras_model(inputs = input_state, outputs = states_hidden2)
  opt = keras::optimizer_adam(lr = 0.0001)
  model %>% compile(
    optimizer = opt,
    loss = "mse",
    metrics = c("accuracy")
    )
  return(list(model = model, input_state = input_state, weights = model$trainable_weights))
}

# both state and action are inputs!
createCriticNetwork = function(state_dim, action_dim) {
  input_state = keras::layer_input(shape = state_dim)
  input_action = keras::layer_input(shape = action_dim, name = "input_action")
  action_hidden = input_action %>%
    layer_dense(units = 30, activation = "linear")
  states_hidden = input_state %>%
    layer_dense(units = 30, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = 30, activation = "linear")
  hiddens = keras::layer_add(c(states_hidden2, action_hidden))
  # outputs compose input + dense layers
  predictions = hiddens %>%
    layer_dense(units = 30, activation = "relu") %>%
    layer_dense(units = action_dim, activation = "linear")
  # create and compile model
  model = keras::keras_model(inputs = c(input_action, input_state), outputs = predictions)
  opt = keras::optimizer_adam(lr = 0.0001)
  model %>% compile(
    optimizer = opt,
    loss = "mse",
    metrics = c("accuracy")
    )
  return(list(model = model, input_action = input_action, input_state = input_state))
}
