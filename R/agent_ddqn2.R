# @title Double Q learning
#
# @format \code{\link{R6Class}} object
# @description
# A \code{\link{R6Class}} to represent Double Deep Q learning Armed Agent
# %$Q_u(S, a; \theta_1) = r + Q_u(S', argmax_a' Q_h(S',a'), \theta_1) + delta$
# target action = argmax Q_h
# @section Methods:
# Inherited from \code{AgentArmed}:
# @inheritSection AgentArmed Methods
#
# @return [\code{\link{AgentDDQN}}].
AgentDDQN = R6::R6Class("AgentDDQN",
  inherit = AgentFDQN,
  public = list(
    p.next.h = NULL,
    setBrain = function() {
      super$setBrain()  # current setBrain will overwrite super$setBrain()
      self$brain_update = self$brain
      self$brain_target = SurroNN$new(self)
    },

    getXY = function(batchsize) {
      self$list.replay = self$mem$sample.fun(batchsize)
      list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
      list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
      self$model = self$brain_update
      self$p.old = self$getYhat(list.states.old)
      self$p.next = self$getYhat(list.states.next)
      self$model = self$brain_target
      self$p.next.h = self$getYhat(list.states.next)
      list.targets = lapply(1:length(self$list.replay), self$extractTarget)
      self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
      temp = simplify2array(list.states.old) # R array put elements columnwise
      mdim = dim(temp)
      norder = length(mdim)
      self$replay.x = aperm(temp, c(norder, 1:(norder - 1)))
      self$replay.y = t(simplify2array(list.targets))  # array p
    },

    extractTarget = function(i) {
      ins = self$list.replay[[i]]
      act2update =  ReplayMem$extractAction(ins)
      yhat = self$p.old[i, ]
      vec.next.Q.u = self$p.next[i, ]    # action selection
      vec.next.Q.h = self$p.next.h[i, ]  # action evaluation
      a_1 = which.max(vec.next.Q.u)  # action selection
      r = ReplayMem$extractReward(ins)
      done = ReplayMem$extractDone(ins)
      if (done) {
        target = r
      } else {
        target = r + self$gamma * vec.next.Q.h[a_1]  # action evaluation
      }
      mt = yhat
      mt[act2update] = target
      return(mt)
    }
  ) # public
)

AgentDDQN$info = function() {
  "Double Deep Q Learning"
}

rlR.conf.AgentDDQN  =  function() {
  RLConf$new(
          render = FALSE,
          console = TRUE,
          log = FALSE,
          policy.maxEpsilon = 1,
          policy.minEpsilon = 0.01,
          policy.decay.rate = exp(-0.001),
          policy.name = "EpsilonGreedy",
          replay.batchsize = 64L,
          agent.update.target.freq  = 200
          )
}

AgentDDQN$test = function() {
  library(rlR)
  env = makeGymEnv("CartPole-v0")
  agent = initAgent("AgentDDQN", env)
  agent$learn(200L)
}
