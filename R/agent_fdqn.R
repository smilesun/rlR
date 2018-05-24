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
    brain.u = NULL,
    initialize = function(env, conf) {
      super$initialize(env, conf)
      self$setBrain()
    },

    setBrain = function() {
      super$setBrain()
      self$brain.u = SurroNN$new(actCnt = self$actCnt, stateDim = self$stateDim, arch.list = self$conf$get("agent.nn.arch"))
    },

      replay = function(batchsize) {
          self$getXY(batchsize)
          self$brain.u$train(self$replay.x, self$replay.y)  # update the policy model
      },

      updateModel = function() {
        self$brain = self$brain.u
      },

      afterStep = function() {
        self$replay(self$replay.size)
      },

      afterEpisode = function(interact) {
        super$afterEpisode(interact)
        self$updateModel()
      }
    ), # public
  private = list(),
  active = list(
    )
  )

AgentFDQN$test = function(iter = 500L, sname = "CartPole-v0", render = TRUE) {
  conf = RLConf$new(
           render = render,
           policy.maxEpsilon = 1,
           policy.minEpsilon = 0.01,
           policy.decay = exp(-0.001),
           policy.name = "EpsilonGreedy",
           replay.batchsize = 64L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = makeGymExperiment(sname = sname, aname = "AgentFDQN", conf = conf)
  perf = interact$run(iter)
  return(perf)
}
