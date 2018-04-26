Agentmlr = R6Class("Agentmlr",
  inherit = AgentDQN,
  public = list(
    initialize = function(actCnt, stateCnt, conf) {
       super$initialize(actCnt, stateCnt, conf)
       self$brain = Surro.mlr$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = surro_fun)
    }
    ), # public
  private = list(),
  active = list(
    )
  )

