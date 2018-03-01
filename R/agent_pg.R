AgentPG = R6Class("AgentPG",
  inherit = AgentArmed,
  public = list(
    initialize = function(actionCnt, stateCnt) {  
      self$brain = SurroDQN$new(actionCnt = actionCnt, stateCnt = stateCnt, fun = NNArsenal$makeNN4PG)
      self$mem = ReplayMem$factory("latest")()
      self$actCnt = actionCnt
    },

    # sample according to the current policy network
    act = function(state) {
      assert(class(state) == "array")
      state = array_reshape(state, c(1L, dim(state)))
      vec.q = self$brain$pred(state)
      sample(x = 0L:(self$actCnt-1L), size = 1L, replace = TRUE, prob = vec.q)
    },
    
    # extract action from replay memory
    extractAct = function(ins) {
      return(ins[[2L]])
    },

    # extract target from one instance of replay memory, which is the one hot encoded action multiplied by the advantage of this episode
    extractTarget = function(ins, advantage) {
        act = self$extractAct(ins)
        temp = rep(0L,self$actCnt)
        temp[act + 1L] =  1L
        label = array(temp, dim = c(1L,self$actCnt))
        mt = label * advantage * (-1) # 'loss' maximization
        return(mt)
    },

    replay = function(batchsize) {
        list.res = self$mem$sample.fun(batchsize)
        list.states = lapply(list.res, ReplayMem$extractOldState)
        list.rewards = lapply(list.res, ReplayMem$extractReward)
        list.targets = lapply(list.res, self$extractTarget, advantage = Reduce(sum, list.rewards))
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

AgentPG$test = function() {

}
