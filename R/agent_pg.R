#' @title
#'
#' @description
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentPG = R6Class("AgentPG",
  inherit = AgentArmed,
  public = list(
    epoch = NULL,
    advantage = NULL,
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
      self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$makeNN4PG)
      self$epoch = conf$get("replay.epoch")
},

    # extract target from one instance of replay memory, which is the one hot encoded action multiplied by the advantage of this episode
    extractTarget = function(ins) {
        act =  ReplayMem$extractAction(ins)
        temp.act = rep(0L, self$actCnt)
        temp.act[act + 1L] =  1L
        label = array(temp.act, dim = c(1L, self$actCnt))
        mt = label * self$advantage * (-1) # 'loss' maximization
        return(mt)
    }

    ), # public
  private = list(),
  active = list(
    )
  )
