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
    grad2a = NULL,
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
    tb.acts = NULL,
    tb.state = NULL,
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

    extractCriticTarget = function(i) {
      y = self$list.rewards[[i]] + self$gamma * self$p.next[i, ]
      return(y)
    },

    # input: state, action
    # output: state-action value
    # target: r_i + gamma Q_{target}(s_new, policy_action)
    trainCritic = function() {
      self$getYhat()
      len = length(self$list.replay)
      list.targets = lapply(1:len, self$extractCriticTarget)
      tb.targets = Reduce(rbind, list.targets)
      self$fitCritic(self$tb.acts, self$tb.state, tb.targets)
    },

    # actor is also with loss mse since action is continous!!
    extractActorTarget = function(i) {
      act = self$list.acts[[i]]
      #FIXME: how to find a target here?
      #target = self$advantage[i, ]
      target = self$advantage[i, ] + self$actor_pred[i, ]
      return(target)
    },

    trainActor = function() {
      self$grad2a = self$brain_critic_update$calGradients2Action(state_input = self$tb.state, action_input = self$tb.acts)
      #FIXME: why the return from tensor flow is a list?
      self$grad2a = self$grad2a[[1L]]
      #FIXME: grad2a should be a scalar!
      self$actor_pred =  self$brain_actor_update$pred(self$tb.state)
      self$advantage = self$grad2a * self$actor_pred
      len = dim(self$replay.x)[1L]
      list.targets = lapply(1:len, self$extractActorTarget)
      tb.targets = Reduce(rbind, list.targets)
      self$fitActor(self$tb.state, tb.targets)
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

    predCritic = function(action_input, state_input) {
      #FIXME: the fixed order of action_input and state_input might be problematic
      res = keras::predict_on_batch(self$brain_critic_update$model, x = list(action_input, state_input))
      return(res)
    },

    fitCritic = function(action_input, state_input, yhat) {
      #FIXME: the fixed order of action_input and state_input might be problematic
      res = keras::fit(self$brain_critic_update$model, x = list(action_input, state_input), y = yhat)
      return(res)
    },

    fitActor = function(state_input, yhat) {
      #FIXME: the fixed order of action_input and state_input might be problematic
      res = keras::fit(self$brain_actor_update$model, x = state_input, y = yhat)
      return(res)
    },

    getYhat = function(...) {
      self$tb.state = Reduce(rbind, self$list.states.old)
      self$tb.acts = Reduce(rbind, self$list.acts)
      self$p.next = self$predCritic(self$tb.acts, self$tb.state)
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

    },

    afterEpisode = function(interact) {
      self$replay(self$replay.size)
    }

))

AgentDDPG$test = function(iter = 1000L, sname = "Pendulum-v0", render = FALSE, console = TRUE) {
  # MountainCarContinuous-v0
  conf = rlR.conf.DQN()
  env = makeGymEnv(sname)
  agent = makeAgent("AgentDDPG", env, conf)
  agent$updatePara(render = render, console = console)
  agent$learn(iter)
}
