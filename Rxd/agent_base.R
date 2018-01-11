AgentArmed = R6Class("AgentArmed",  # agent do choose between arms
  public = list(
    epsilon = NULL,
    brain = NULL,  # a table or a function approximator to represent the value function
    mem = NULL,  # replay memory
    actCnt = NULL,
    glogger = NULL,
    yhat = NULL, 
    decorate = function(x){x},  # transform the  action space since some Gym environment has non-continous feasible actions
    initialize = function(brain, mem, actCnt, decorator = function(x){x}) {  # mem is an ReplayMem object
      self$brain = brain
      self$mem = mem
      self$actCnt = actCnt
      self$decorate = decorator  # some Gym scenario have noncontinous actions
    },

    # transform observation to  the replay memory
    observe = function(state.old, action, reward, state.new, action.new = NULL, delta = NULL, context = NULL) {
      # state, action, accumulated reward
      ins = ReplayMem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, delta = NULL)
      delta = self$calculateTDError(ins)
      ins$deltaOfdelta = delta - ins$delta  # calculate the TD error difference 
      ins$deltaOfdeltaPercentage = abs(ins$deltaOfdelta)/abs(ins$delta)  # calculate the TD error difference 
      ins$delta = delta
      self$glogger$log.nn$info("experience tuple:sars_delta_delta2_delta2_percent :%s", paste0(ins))
      self$mem$add(ins)
    },

    calculateTDError = function(ins) {
      vec.mt = self$extractTarget(ins)  # vector of target, self$yhat is calculated inside. Usually extractTarget is applied to a batch of x space instances and return the Bellman equation target, here it only extract one x space instance
      mean((vec.mt - self$yhat)^2)
    },

    extractTarget = function(ins) {
      stop("not implemented")
    },

    #' model update only works
    
    replay = function(batchsize) {
        stop("not implemented")
    }
    ), # public
  private = list(),
  active = list(
    randomAct = function() {
      sample.int(self$actCnt)[1L] -1L  # OpenAI Gym  convention
    }
    )
  )


