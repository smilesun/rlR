AgentFreeRun = R6Class("AgentFreeRun",
  inherit = AgentArmed,
  public = list(
    initialize = function(actCnt, stateCnt, conf) {
       super$initialize(actCnt, stateCnt, conf)
       self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, arch.list = conf$get("agent.nn.arch"))
       self$model = self$brain
    }
  )
)
