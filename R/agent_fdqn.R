# @title Frozen target Q learning
#
# @format \code{\link{R6Class}} object
# @description Frozen target Q learning
#
# @section Methods:
# Inherited from \code{AgentArmed}:
# @inheritSection AgentArmed Methods
#
# @return [\code{\link{AgentFDQN}}].
AgentFDQN = R6::R6Class("AgentFDQN",
  inherit = AgentDQN,
  public = list(
    brain_target = NULL,
    brain_update = NULL,
    last_update = NULL,
    initialize = function(env, conf) {
      self$last_update = 0
      super$initialize(env, conf)
      self$updateFreq = self$conf$get("agent.update.target.freq")
    },

    setBrain = function() {
      super$setBrain()
      self$brain_update = SurroNN$new(self)
      self$brain_target = self$brain
    },

    showBrain = function() {
      print("control network:")
      print(self$brain_update$model)
      print("target network:")
      print(self$brain_target$model)
    },

    ## @override
    getXY = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
        self$model = self$brain_update  # use update network to generate target
        self$p.old = self$getYhat(list.states.old)
        self$model = self$brain_target  # use target network to generate target
        self$p.next = self$getYhat(list.states.next)
        list.targets = lapply(1:length(self$list.replay), self$extractTarget)
        #temp = Reduce(rbind, list.states.old)  # does not work for tensor
        batch_states = simplify2array(list.states.old) # R array put elements columnwise
        mdim = dim(batch_states)
        norder = length(mdim)
        self$replay.x = aperm(batch_states, c(norder, 1:(norder - 1)))
        self$replay.y = t(simplify2array(list.targets))  # array put elements columnwise
        #diff_table = abs(self$replay.y - self$p.old)
        #self$replay_delta = apply(diff_table, 1, mean)
    },


    replay = function(batchsize) {
      self$getXY(batchsize)  # from base class
      self$brain_update$train(self$replay.x, self$replay.y)  # update the policy model
    },

    act = function(state) {
      assert(class(state) == "array")
      self$model = self$brain_update
      self$evaluateArm(state)
      self$policy$act(state)
    },

    updateModel = function() {
      cat(sprintf("\n\n updating model \n\n"))
      tw = self$brain_target$getWeights()
      uw = self$brain_update$getWeights()
      uuw = lapply(uw, function(x) x * 0.1)
      ttw = lapply(tw, function(x) x * 0.9)
      ww = mapply("+", uw, tw)
      self$brain_target$setWeights(uw)
      self$last_update = self$interact$global_step_len
    },

    shouldUpdateModel = function() {
      self$interact$global_step_len - self$last_update > self$updateFreq
    },

    afterEpisode = function() {
      if (self$shouldUpdateModel()) {
        self$updateModel()
      }
      super$afterEpisode()
    }
  )
)

rlR.conf.AgentFDQN  =  function() {
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



AgentFDQN$info = function() {
 "Frozen Target Deep Q Learning"
}

AgentFDQN$test = function() {
  library(rlR)
  env = makeGymEnv("CartPole-v0")
  agent = initAgent("AgentFDQN", env)
  agent$learn(400L)
}
