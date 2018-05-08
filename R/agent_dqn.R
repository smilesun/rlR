AgentDQN = R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    initialize = function(actCnt, stateCnt, conf) {
       super$initialize(actCnt, stateCnt, conf)
       self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$dqn, conf$get("agent.nn.arch"))
    },

    extractTarget = function(ins) {
        act2update =  ReplayMem$extractAction(ins)
        old.state = ReplayMem$extractOldState(ins)
        old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))  # array could have scalar dim
        p.old = self$brain$pred(old.state)
        self$yhat = p.old  # for calculating the  TD error
        next.state = ReplayMem$extractNextState(ins)
        next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
        vec.next.Q = self$brain$pred(next.state)
        a_1 = which.max(vec.next.Q)  # action index start from 1L
        r = ReplayMem$extractReward(ins)
        target = r + self$gamma * max(vec.next.Q)
        mt = p.old
        mt[act2update + 1L] = target  # the not active action arm's Q will not be updated
        return(mt)
    },

    afterStep = function() {
          self$replay(self$replay.size)
    },

    afterEpisode = function(interact) {
          self$policy$afterEpisode()
          self$mem$afterEpisode()
    }
    ), # public
  private = list(),
  active = list(
    )
  )

dqn_cart_pole = function(iter = 500L) {
  conf = rlR::RLConf$new(
           policy.epsilon = 1,
           policy.decay = exp(-0.2),
           policy.name = "EpsilonGreedy",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(sname = "CartPole-v0", aname = "AgentDQN", conf = conf)
  perf = interact$run(iter)
  return(perf)
}

dqn_cart_pole1 = function(iter = 500L) {
  conf = rlR::RLConf$new(
           policy.epsilon = 1,
           policy.decay = exp(-0.2),
           policy.name = "EpsilonGreedy",
           replay.memname = "PrioritizedRank",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(sname = "CartPole-v0", aname = "AgentDQN", conf = conf)
  perf = interact$run(iter)
  return(perf)
}
