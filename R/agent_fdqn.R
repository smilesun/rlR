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
      self$brain_update = SurroNN$new(actCnt = self$actCnt, stateDim = self$stateDim, arch.list = self$conf$get("agent.nn.arch"))
      self$brain_target = self$brain
    },

      replay = function(batchsize) {
          self$model = self$brain_target  # use target network to generate target
          self$getXY(batchsize)
          self$brain_update$train(self$replay.x, self$replay.y)  # update the policy model
          #w1 = self$brain_target$getWeights()
          #print("replay")
          #w2 = self$brain$getWeights()
          #w3 = self$brain_update$getWeights()
          #print(all.equal(w1,w2))
          #print(all.equal(w1,w3))
      },
   
      act = function(state) {
        assert(class(state) == "array")
        self$model = self$brain_update
        self$evaluateArm(state)
        self$policy$act(state)
      },

      # 
      updateModel = function() {
        uw = self$brain_update$getWeights()
        self$brain_target$setWeights(uw)
        #self$brain_target = self$brain_update$clone(deep = TRUE)
        #print(self$brain_target$getWeights())
        #print("update model")
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


AgentFDQN$test = function(iter = 1000L, sname = "CartPole-v0", render = TRUE, console = FALSE) {
  conf = rlR.conf.DQN()
  conf$updatePara("console", console)
  conf$updatePara("render", render)
  interact = makeGymExperiment(sname = sname, aname = "AgentFDQN", conf = conf)

  perf = interact$run(iter)
  return(perf)
}
