#' @title Actor Critic Agent
#'
#' @description Actor Critic Agent
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentActorCritic = R6Class("AgentActorCritic",
  inherit = AgentPG,
  public = list(
    brain_actor = NULL,  # cross entropy loss
    brain_critic = NULL, # mse loss
    critic_yhat = NULL,
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt, stateCnt, conf = conf)
      self$brain_actor = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$dqn, conf$get("agent.nn.arch"))
      self$brain_critic = SurroNN$new(actCnt = 1L, stateCnt = self$stateCnt, fun = NNArsenal$dqn, conf$get("agent.nn.arch.critic"))
      },

    calculateTDError = function(ins) {
      vec.mt = self$extractCriticTarget(ins)
      err = vec.mt - self$critic_yhat
      mean(err ^ 2)
    },

     replay = function(batchsize) {
          list.res = self$mem$sample.fun(batchsize)
          list.states = lapply(list.res, ReplayMem$extractOldState)
          ded = cumprod(rep(0.99, batchsize))
          list.targets.actor = lapply(list.res, self$extractActorTarget)
          list.targets.actor = lapply(1:batchsize, function(x) list.targets.actor[[x]] * ded[x])
          list.targets.critic = lapply(list.res, self$extractCriticTarget)
          x = as.array(t(as.data.table(list.states)))  # array put elements columnwise
          y_actor = rbindlist(lapply(list.targets.actor, as.data.table))
          y_actor = as.data.frame(y_actor)
          y_actor = as.matrix(y_actor)
          y_critic = rbindlist(lapply(list.targets.critic, as.data.table))
          y_critic = as.data.frame(y_critic)
          y_critic = as.matrix(y_critic)
          self$brain_actor$train(x, y_actor)  # update the policy model
          self$brain_critic$train(x, y_critic)  # update the policy model
      },

      extractCriticTarget = function(ins) {
          next.state = ReplayMem$extractNextState(ins)
          next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
          next.V = self$brain_critic$pred(next.state)
          self$critic_yhat = next.V
          r = ReplayMem$extractReward(ins)
          y = r + self$conf$get("agent.gamma") * next.V
          return(y)
      },

      extractActorTarget = function(ins) {
          act = ReplayMem$extractAction(ins)
          old.state = ReplayMem$extractOldState(ins)
          old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))
          critic.old.v = self$brain_critic$pred(old.state)
          next.state = ReplayMem$extractNextState(ins)
          next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
          critic.next.v = self$brain_critic$pred(next.state)
          r = ReplayMem$extractReward(ins)
          advantage = r + self$conf$get("agent.gamma") * critic.next.v - critic.old.v
          advantage = (+1) * as.vector(advantage)  # convert (1,1) matrix to scalar
          vec.act = rep(0L, self$actCnt)
          vec.act[act + 1L] = 1L # the not active action will have exact label
          target = advantage * array(vec.act, dim = c(1L, self$actCnt))
          return(target)
    },

    afterStep = function() {
    },

    afterEpisode = function(interact) {
        episode.idx = interact$perf$epi.idx
        total.step = unlist(interact$perf$list.stepsPerEpisode)[episode.idx]
        self$replay(total.step)
        self$policy$afterEpisode()
        self$mem$afterEpisode()
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain_actor$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    }
    ), # public
  private = list(),
  active = list(
    )
  )

a3c_cart_pole0 = function(iter = 200L) {
  conf = rlR::RLConf$new(
           agent.name = "AgentActorCritic",
           policy.name = "EpsilonGreedy",
           policy.epsilon = 1,
           policy.decay = exp(-0.05),
           replay.memname = "Latest",
           agent.nn.arch = list(nhidden = 64, act1 = "tanh", act2 = "softmax", loss = "categorical_crossentropy", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0)", bias_regularizer = "regularizer_l2(l=0.0)"),
          agent.nn.arch.critic = list(nhidden = 64, act1 = "tanh", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)")
           )
  interact = rlR::makeGymExperiment(sname = "CartPole-v0", "AgentActorCritic", conf = conf)
  perf = interact$run(iter)
  return(perf)
}

a3c_cart_pole1 = function(iter = 50L) {
  conf = rlR::RLConf$new(
           agent.name = "AgentActorCritic",
           policy.name = "EpsilonGreedy",
           replay.memname = "LatestProb",
           replay.batchsize = 5L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "softmax", loss = "categorical_crossentropy", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0)", bias_regularizer = "regularizer_l2(l=0.0)"),
          agent.nn.arch.critic = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)")
           )
  interact = rlR::makeGymExperiment(sname = "CartPole-v0", "AgentActorCritic", conf = conf)
  perf = interact$run(iter)
  return(perf)
}

a3c_cart_pole2 = function(iter = 50L) {
  conf = rlR::RLConf$new(
           agent.name = "AgentActorCritic",
           policy.name = "EpsilonGreedy",
           policy.epsilon = 1,
           policy.decay = exp(-0.25),
           policy.minEpsilon = 0,
           replay.memname = "LatestProb",
           replay.batchsize = 5L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "softmax", loss = "categorical_crossentropy", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0)", bias_regularizer = "regularizer_l2(l=0.0)"),
          agent.nn.arch.critic = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)")
           )
  interact = rlR::makeGymExperiment(sname = "CartPole-v0", "AgentActorCritic", conf = conf)
  perf = interact$run(iter)
  return(perf)
}

