AgentArmed = R6Class("AgentArmed",  # agent do choose between arms
  public = list(
    epsilon = NULL,
    brain = NULL,  # a table or a function approximator to represent the value function
    mem = NULL,  # replay memory
    actCnt = NULL,
    initialize = function(brain, mem, actCnt) {  # mem is an ReplayMem object
      self$brain = brain
      self$mem = mem
      self$actCnt = actCnt
    },

    # transform observation to  the replay memory
    observe = function(state.old, action, reward, state.new, action.new = NULL) {
      # state, action, accumulated reward
      self$mem$add(list(state.old = state.old, action = action, reward = reward, state.new = state.new))
    },

    extractOldState = function(x) {
      return(x[[1L]])
    },

    extractNextState = function(x) {
      return(x[[4L]])
    },

    extractReward = function(x) {
      return(x[[3L]])
    },

    extractTarget = function(ins) {
      stop("not implemented")
    },

    #' model update only works
    
    replay = function(batchsize) {
        list.res = self$mem$ins.sample.all(batchsize)
        list.states = lapply(list.res, self$extractOldState)
        list.targets = lapply(list.res, self$extractTarget)
        x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage
        y = array(unlist(list.targets), dim = c(length(list.targets), self$actCnt))
        # y = array_reshape(y, dim = c(1L, dim(y)))
        self$brain$train(x, y)  # update the policy model
    }
    ), # public
  private = list(),
  active = list(
    randomAct = function() {
      sample.int(self$actCnt)[1L] -1L
    }
    )
  )


