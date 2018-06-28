#' @title Frozen target Q learning
#'
#' @format \code{\link{R6Class}} object
#' @description Frozen target Q learning
#'
#' @section Methods:
#' Inherited from \code{AgentArmed}:
#' @inheritSection AgentArmed Methods
#'
#' @return [\code{\link{AgentFDQN}}].
#' @export
AgentFDQN = R6::R6Class("AgentFDQN",
  inherit = AgentDQN,
  public = list(
    brain_target = NULL,
    brain_update = NULL,
    initialize = function(env, conf) {
      super$initialize(env, conf)
      self$setBrain()
    },

    setBrain = function() {
      super$setBrain()
      self$brain_update = SurroNN$new(self)
      self$brain_target = self$brain
      print("control network:")
      print(self$brain_update$model)
      print("target network:")
      print(self$brain_target$model)
    },

    replay = function(batchsize) {
      self$model = self$brain_target  # use target network to generate target
      self$getXY(batchsize)
      self$brain_update$train(self$replay.x, self$replay.y)  # update the policy model
    },

    act = function(state) {
      assert(class(state) == "array")
      self$model = self$brain_update
      self$evaluateArm(state)
      self$policy$act(state)
    },

    updateModel = function() {
      uw = self$brain_update$getWeights()
      self$brain_target$setWeights(uw)
    },

    afterEpisode = function(interact) {
      super$afterEpisode(interact)
      self$updateModel()
    }
  )
)


AgentFDQN$test = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  conf = rlR.conf.DQN()
  conf$updatePara("console", console)
  conf$updatePara("render", render)
  interact = makeGymExperiment(sname = sname, aname = "AgentFDQN", conf = conf)
  perf = interact$run(iter)
  return(perf)
  #
  env = makeGymEnv(sname)
  agent = makeAgent("AgentFDQN", env)
  agent$updatePara(console = console, render = render)
  agent$learn(iter)

}

AgentFDQN$testCnn = function(iter = 1000, render = FALSE) {
  env = makeGymEnv("Pong-v0", act_cheat = c(2, 3), repeat_n_act = 4)
  agent = makeAgent("AgentFDQN", env)
  agent$updatePara(replay.batchsize = 32, render = render, replay.freq = 4L)
  agent$learn(iter)
}
