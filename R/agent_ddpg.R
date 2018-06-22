#' @title Deep Deterministic Policy Gradient
#'
#' @format \code{\link{R6Class}} object
#' @description
#' @section Methods:
#'
#' @return [\code{\link{AgentDDPG}}].
#' @export
AgentDDPG = R6::R6Class("AgentDDPG",
  inherit = AgentArmed,
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
      self$task = "actor.update"
      self$brain_actor_update = SurroNN$new(self)
    },

    trainCritic = function() {
    },

    replay = function(size) {
      self$trainCritic(self$replay.x, self$replay.actions)
      #self$brain_critic_update$train(self$replay.x, y_critic)  # first update critic
      #self$brain_actor_update$train(self$replay.x, y_actor)
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
      taw = self$brain_actor_target$getWeights()
      self$brain_actor_target$setWeights(self$tau * uaw + (1.0 - self$tau) * taw)
      # critic
      uaw = self$brain_critic_update$getWeights()
      taw = self$brain_critic_target$getWeights()
      self$brain_critic_target$setWeights(self$tau * uaw + (1.0 - self$tau) * taw)
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
