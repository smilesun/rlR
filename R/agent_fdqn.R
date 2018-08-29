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
      self$updateFreq = self$conf$get("agent.update.target.freq")
      self$setBrain()
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

    replay = function(batchsize) {
      self$model = self$brain_target  # use target network to generate target
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
      uw = self$brain_update$getWeights()
      self$brain_target$setWeights(uw)
    },

    shouldUpdateModel = function() {
      self$interact$global_step_len %% self$updateFreq == 0
    },

    afterStep = function() {
      super$afterStep()
      #if (!is.null(self$updateFreq)) {
        if (self$shouldUpdateModel()) {
          self$updateModel()
        }
      #}
    },

    afterEpisode = function(interact) {
      super$afterEpisode(interact)
      # by default, update model after each episode
      #if (is.null(self$updateFreq)) {
      #  self$updateModel()
      #}
    }
  )
)
