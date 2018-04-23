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
      self$brain = SurroDQN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$makeNN4PG)
},

    # sample according to the current policy network
    act = function(state) {
      assert(class(state) == "array")
      state = array_reshape(state, c(1L, dim(state)))
      vec.q = self$brain$pred(state) #FIXME: NA output
      act = sample(x = 0L:(self$actCnt - 1L), size = 1L, replace = TRUE, prob = vec.q)
      self$glogger$log.nn$info("act index %d taken(0)", act)
      return(act)
    },

    # extract action from replay memory
    extractAct = function(ins) {
      return(ins[[2L]])
    },

    setAdvantage = function(adv) {
      self$advantage = adv
    },

    # extract target from one instance of replay memory, which is the one hot encoded action multiplied by the advantage of this episode
    extractTarget = function(ins) {
        act = self$extractAct(ins)
        temp = rep(0L, self$actCnt)
        temp[act + 1L] =  1L
        label = array(temp, dim = c(1L, self$actCnt))
        # self$advantage = self$getAdvantage()
        mt = label * self$advantage * (-1) # 'loss' maximization
        return(mt)  # mt = my target
    },

    replay = function(batchsize) {
        list.res = self$mem$sample.fun(batchsize)
        list.states = lapply(list.res, ReplayMem$extractOldState)
        list.targets = lapply(list.res, self$extractTarget)
        x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage
        y = array(unlist(list.targets), dim = c(length(list.targets), self$actCnt))
        # y = array_reshape(y, dim = c(1L, dim(y)))
        self$brain$train(x, y)  # update the policy model
    }

    ), # public
  private = list(),
  active = list(
    )
  )

