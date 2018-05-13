#' @title Frozen target Q learning
#'
#' @description Frozen target Q learning
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentFDQN = R6Class("AgentFDQN",
  inherit = AgentDQN,
  public = list(
    brain.u = NULL,
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt, stateCnt, conf)
      self$brain.u = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, arch.list = conf$get("agent.nn.arch"))
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

fdqn_cart = function(iter = 500L) {
  conf = rlR::RLConf$new(
           policy.epsilon = 1,
           policy.minEpsilon = 0.01,
           policy.decay = exp(-0.001),
           policy.name = "EpsilonGreedy",
           replay.batchsize = 64L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(sname = "CartPole-v0", aname = "AgentFDQN", conf = conf)
  perf = interact$run(iter)
  return(perf)
}
