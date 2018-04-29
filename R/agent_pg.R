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
    advantage = NULL,
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
      self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$makeNN4PG)
},

    # sample according to the current policy network
    act = function(state) {
      assert(class(state) == "array")
      state = array_reshape(state, c(1L, dim(state)))
      vec.q = self$brain$pred(state)
      act = sample(x = 0L:(self$actCnt - 1L), size = 1L, replace = TRUE, prob = vec.q)
      self$glogger$log.nn$info("act index %d taken(0)", act)
      return(act)
    },

    setAdvantage = function(adv) {
      self$advantage = adv
    },

    # extract target from one instance of replay memory, which is the one hot encoded action multiplied by the advantage of this episode
    extractTarget = function(ins) {
        act =  ReplayMem$extractAction(ins)
        temp = rep(0L, self$actCnt)
        temp[act + 1L] =  1L
        label = array(temp, dim = c(1L, self$actCnt))
        mt = label * self$advantage * (-1) # 'loss' maximization
        return(mt)  # mt = my target
    },

    replay = function(batchsize) {
        list.x.y = self$getXY(batchsize)
        x = list.x.y$x
        y = list.x.y$y
        self$brain$train(x, y)  # update the policy model
    }
    ), # public
  private = list(),
  active = list(
    )
  )

