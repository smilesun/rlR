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
    epi.idx = NULL,
    actCnt = NULL,
    stateCnt = NULL,
    conf = NULL,
    vec.arm.q = NULL,      # store Q value for each arm
    random.action = NULL,  # store random.action
    epsilon = NULL,  # policy_fun currently do not have this parameter
    # built from conf
    glogger = NULL,
    mem = NULL,  # replay memory
    policy = NULL,
    gamma = NULL,
    # for init in other child class
    brain = NULL,  # a table or a function approximator to represent the value function
    yhat = NULL,  # bellman equation estimation
    # member function
    ins2String = function(x){x},   # function must be defined in this way
    armremap = function(x){x},  # transform the  action space since some Gym environment has non-continous feasible actions
    # constructor
    initialize = function(actCnt, stateCnt, conf) {
      self$epi.idx = 1L
      self$actCnt = actCnt
      self$stateCnt = stateCnt
      self$conf = conf
      self$vec.arm.q = vector(mode = "numeric", length = self$actCnt) 
      self$epsilon = self$conf$get("policy.epsilon")
      self$gamma = self$conf$get("agent.gamma")
      #
      self$glogger = RLLog$new(conf)
      memname = conf$get("replay.memname")
      self$mem = ReplayMem$factory(memname)(conf = conf)
      policy_fun = conf$get("policy.name")
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
      vec.mt = self$extractTarget(ins)  
      err = vec.mt - self$yhat
      mean(err ^ 2)
    },

    updateDT = function(x,y) {
      yhat = self$brain$pred(x)
      self$mem$updateDT(yhat, y)
    },

    extractTarget = function(ins) {
      stop("not implemented")
    },

    setAdvantage = function(adg) {
      # by default do nothing
    },

    replay = function(batchsize) {
        stop("not implemented")
    }
    ), # public
  private = list(),
  active = list(
    randomAct = function() {
      sample.int(self$actCnt)[1L] - 1L  # OpenAI Gym  convention
    }
    )
  )

