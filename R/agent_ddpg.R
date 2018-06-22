#' @title Deep Deterministic Policy Gradient
#'
#' @format \code{\link{R6Class}} object
#' @description
#' @section Methods:
#'
#' @return [\code{\link{AgentDDPG}}].
#' @export
AgentDDPG = R6::R6Class("AgentDDPG",
  inherit = AgentActorCritic,
  public = list(
    tau = NULL,  # bilinear combination of target and update network
    model = NULL,
    task = NULL,
    input_action_update = NULL,
    input_state_update = NULL,
    tensor_input_state_actor_update  = NULL,
    input_actor_update_weights = NULL,
    brain_actor_update = NULL,
    brain_critic_update = NULL,
    brain_actor_target = NULL,
    brain_critic_target = NULL,
    replay_actions = NULL,
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
        self$tensor_input_state_actor_update = tuple$input_state
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

    trainCritic = function() {
      # self$replay.x, self$replay.actions
    },

    trainActor = function() {
      # 
    },

    replay = function(size) {
      self$unpack(size)
      self$trainCritic()
      self$trainActor()
      self$updateModel()
    },

    evaluateArm = function(state) {
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

    unpack = function(batchsize) {
      self$list.replay = self$mem$sample.fun(batchsize)
      list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
      list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
      self$list.rewards = lapply(self$list.replay, ReplayMem$extractReward)
      self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
      temp = simplify2array(list.states.old) # R array put elements columnwise
      mdim = dim(temp)
      norder = length(mdim)
      self$replay.x = aperm(temp, c(norder, 1:(norder - 1)))
    },

    afterStep = function() {

    },

    afterEpisode = function(interact) {
      self$replay(self$replay.size)
    }

))

AgentDDPG$test = function(iter = 1000L, sname = "Pendulum-v0", render = TRUE, console = FALSE) {
  conf = rlR.conf.DQN()
  env = makeGymEnv(sname)
  agent = makeAgent("AgentDDPG", env, conf)
  agent$updatePara(render = TRUE)
  agent$learn(1)
}


createActorNetwork = function(state_dim = 784, action_dim = 1L) {
  input_state = keras::layer_input(shape = state_dim)
  states_hidden = input_state %>%
    layer_dense(units = 300, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = 300, activation = "linear") %>%
    layer_dense(units = 1, activation = "linear")
  model = keras::keras_model(inputs = input_state, outputs = states_hidden2)
  opt = keras::optimizer_adam(lr = 0.0001)
  model %>% compile(
    optimizer = opt,
    loss = "mse",
    metrics = c("accuracy")
    )
  return(list(model = model, input_state = input_state, weights = model$trainable_weights))
}

createCriticNetwork = function(state_dim, action_dim) {
  input_state = keras::layer_input(shape = state_dim)
  input_action = keras::layer_input(shape = action_dim, name = "input_action")
  action_hidden = input_action %>%
    layer_dense(units = 300, activation = "linear")
  states_hidden = input_state %>%
    layer_dense(units = 300, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = 300, activation = "linear")
  hiddens = keras::layer_add(c(states_hidden2, action_hidden))
  # outputs compose input + dense layers
  predictions = hiddens %>%
    layer_dense(units = 300, activation = "relu") %>%
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
