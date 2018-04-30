#' @title
#'
#' @description
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
      self$brain.u = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$dqn, conf$get("agent.nn.arch"))
    },

      replay = function(batchsize) {
          list.x.y = self$getXY(batchsize)
          x = list.x.y$x
          y = list.x.y$y
          self$brain.u$train(x, y)  # update the policy model
      },

      updateModel = function() {
        self$brain = self$brain.u
      }

    ), # public
  private = list(),
  active = list(
    )
  )

