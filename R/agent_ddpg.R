#' @title Deep Deterministic Policy Gradient
#'
#' @format \code{\link{R6Class}} object
#' @description Continous action agent
#' Inherited from \code{AgentActorCritic}:
#' @section Methods:
#' @inheritSection AgentArmed Methods
#'
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
    tb.acts = NULL,  # acts from replay memory
    tb_acts_target_policy = NULL,  # acts according to policy with respect to states
    tb_acts_update_policy = NULL,  # acts according to policy with respect to states
    tb.state = NULL,
    tb_state_new = NULL,
    tb.targets = NULL,
    initialize = function(env, conf) {
      self$tau = 0.1
      super$initialize(env, conf)
      self$setBrain()
    },

    createBrain = function() {
      if (self$task == "critic.update") {
        tuple = createCriticNetwork(state_dim = self$stateDim, action_dim = 1L)
        self$input_action_update = tuple$input_action
        self$input_state_update = tuple$input_state
        return(tuple$model)
      } else if (self$task == "actor.update"){
        tuple = createActorNetwork(state_dim = self$stateDim, action_dim = 1L)
        self$input_state_actor_update = tuple$input_state
        self$input_actor_update_weights = tuple$weights
        return(tuple$model)
      }
    },

    setBrain = function() {
      self$task = "critic.update"
      self$brain_critic_update = SurroNN$new(self)
      self$brain_critic_target = SurroNN$new(self)
      self$task = "actor.update"
      self$brain_actor_update = SurroNN$new(self)
      self$brain_actor_target = SurroNN$new(self)
      self$model = self$brain_critic_update
    },

    # target: r_i + gamma Q_{target}(s_new, policy_action)
    # policy_action is generated from the target policy network
    extractCriticTarget = function(i) {
      y = self$list.rewards[[i]] + self$gamma * self$p.next[i, ]
      return(y)
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
      if (dim(self$tb.targets) != 1) {
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
      #######
      #x <- 1:3
      #y <- 4:6
      #mapply(list, x, y, SIMPLIFY=F) # gives a list of 3 tuples
      #mapply(c, x, y, SIMPLIFY=F) # gives a list of 3 tuples
      #######
      opt = tf$train$AdamOptimizer(0.001)
      self$optimize = opt$apply_gradients(grad_and_vars)
    },

    trainActorSess = function(state_input, input_criticQ2act) {
      self$trainActorSessInit()
      sname = self$brain_actor_update$model$input$name
      aname = self$ph_critic2act$name
      np = reticulate::import("numpy", convert = FALSE)
      sstate = np$array(state_input)
      scritic2act = np$array(input_criticQ2act)
      feed_dict = py_dict(c(sname, aname), c(sstate, scritic2act))
      self$sess$run(tf$initialize_all_variables())
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
      if (dim(self$tb.targets) != 1) {
      self$trainActorPrepare()
      self$trainActorSess(self$tb.state, self$grad2a)
      self$updateModel()
      }
    },

    evaluateArm = function(state) {
      #FIXME: plus noise here
      self$vec.arm.q = self$brain_actor_update$pred(state)
    },

    act = function(state) {
      checkmate::assert_array(state)
      state = array_reshape(state, c(1, self$stateDim))
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
      res = keras::fit(self$brain_critic_update$model, x = list(action_input, state_input), y = yhat, epochs = 1)
      return(res)
    },

    fitActor = function(state_input, yhat) {
      #FIXME: the fixed order of action_input and state_input might be problematic
      res = keras::fit(self$brain_actor_update$model, x = state_input, y = yhat)
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
      self$replay(self$replay.size)
    },

    afterEpisode = function(interact) {
    }

))

AgentDDPG$test = function(iter = 30, sname = "Pendulum-v0", render = FALSE, console = TRUE) {
  # MountainCarContinuous-v0
  conf = rlR.conf.DQN()
  env = makeGymEnv(sname)
  agent = makeAgent("AgentDDPG", env, conf)
  agent$updatePara(render = render, console = console)
  agent$learn(iter)
}
