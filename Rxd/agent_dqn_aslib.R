AgentDQNAslib = R6Class("AgentDQNAslib",
  inherit = AgentDQN,
  public = list(
    continueFlag = FALSE,
    actionCount = NULL,
    initialize = function(actionCnt, stateCnt, fun, memname = "latest") {
      self$actionCount = vector(mode = "integer", length = actionCnt)
       super$initialize(actionCnt = actionCnt, stateCnt=stateCnt, fun = fun, memname = memname)
    },
    act = function(state) {
      assert(class(state) == "array")
      state = array_reshape(state, c(1L, dim(state)))
      log.nn$info("state: %s", paste(state, collapse = ' '))
      vec.q = self$brain$pred(state)
      log.nn$info("prediction: %s", paste(vec.q, collapse = ' '))
      action = which.max(vec.q) - 1L  # always use OpenAI gym convention
      log.nn$info("chosen %d", action)
      if((runif(1L) < self$epsilon)|| self$continueFlag) {
        self$continueFlag = TRUE
        a2 = self$randomAct
        log.nn$info("random chosen %d", a2)
        self$actionCount[a2 + 1L] = self$actionCount[a2 + 1L] + 1L
        return(a2)
      }
      self$actionCount[action + 1L] = self$actionCount[action + 1L] + 1L
      return(action)
    }
    ), # public
  active = list(
    randomAct = function() {
      sample.int(self$actCnt)[1L] -1L
    }
    )
    )
  
AgentDQNAslib2 = R6Class("AgentDQNAslib2",
  inherit = AgentDQNAslib,
  public = list(
    initialize = function(actionCnt, stateCnt, fun, memname = "latest") {
      self$actionCount = vector(mode = "integer", length = actionCnt)
       super$initialize(actionCnt = actionCnt, stateCnt=stateCnt, fun = fun, memname = memname)
    }),
  active = list(
    randomAct = function() {
      which.min(self$actionCount) -1L
    })
    )
  

