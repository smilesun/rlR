Agentmlr = R6Class("Agentmlr",
  inherit = AgentDQN,
  public = list(
    initialize = function(actCnt, stateCnt, conf) {
       super$initialize(actCnt, stateCnt, conf)
       self$brain = Surro.mlr$new(actCnt = self$actCnt, stateCnt = self$stateCnt) # wipe out self$brain
    },

    replay = function(batchsize) {
        list.x.y = self$getXY(batchsize)
        # debug: self$brain$pred(x)
        x = list.x.y$x
        y = list.x.y$y
        self$brain$train(x, y, act = self$list.acts)  # update the policy model
        self$updateDT(x, y)
    }
    ), # public
  private = list(),
  active = list(
    )
  )

