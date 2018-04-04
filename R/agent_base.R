#' @title 
#' 
#' @description
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
AgentArmed = R6Class("AgentArmed",  # agent do choose between arms
  public = list(
    # constructor init
    actCnt = NULL,
    stateCnt = NULL,
    conf = NULL,
    vec.arm.q = NULL,      # store Q value for each arm
    # built from conf
    glogger = NULL,
    mem = NULL,  # replay memory
    policy = NULL,
    # for init in other child class
    brain = NULL,  # a table or a function approximator to represent the value function
    yhat = NULL,  # bellman equation estimation
    # member function
    ins2String = function(x){x},   # function must be defined in this way
    armremap = function(x){x},  # transform the  action space since some Gym environment has non-continous feasible actions
    # constructor
    initialize = function(actCnt, stateCnt, conf) {
      self$actCnt = actCnt
      self$stateCnt = stateCnt
      self$conf = conf
      self$vec.arm.q = vector(mode = "numeric", length = self$actCnt) 
      #
      self$glogger = RLLog$new(conf)
      memname = conf$static$agent$memname
      self$mem = ReplayMem$factory(memname)(conf = conf)
      policy_fun = conf$static$agent$policy
      self$policy = PolicyFactory$make(policy_fun, self)
    },

    # transform observation to  the replay memory
    observe = function(state.old, action, reward, state.new, action.new = NULL, delta = NULL, context = NULL) {
      ins = ReplayMem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, delta = NULL)
      delta = self$calculateTDError(ins)
      ins$delta = delta
      ins$deltaOfdelta = NA
      ins$deltaOfdeltaPercentage = NA
      ins$context = context  # for extension
      self$glogger$log.nn$info("agentbase: observing new experience tuple:sars_delta_delta2_delta2_percent :%s", self$ins2String(ins))
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


