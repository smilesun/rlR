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
    random.cnt = NULL,
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
      self$random.cnt = 0L
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
    },

    list2df = function () {
      
    },

    getXY = function(batchsize) {
        list.res = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        for (i in self$mem$replayed.idx) {
          self$glogger$log.nn$info("%s", self$mem$samples[[i]])
        }
        list.states = lapply(list.res, ReplayMem$extractOldState)
        list.targets = lapply(list.res, self$extractTarget)  # target will be different at each iteration for the same experience
        self$list.acts = lapply(list.res, ReplayMem$extractAction)
        x = as.array(t(as.data.table(list.states)))  # array put elements columnwise
        y = rbindlist(lapply(list.targets, as.data.table))
        y = as.data.frame(y)
        y = as.matrix(y)
        return(list(x = x, y = y))
    },

    act = function(state) {
      assert(class(state) == "array")
      self$evaluateArm(state)  # calculation will be used for the policy to decide which arm to use
      act = self$policy(state)  # returning the chosen action
      self$glogger$log.nn$info("action: %d", act)
      return(act)
    }

    ), # public
  private = list(),
  active = list(
    randomAct = function() {
      sample.int(self$actCnt)[1L] - 1L  # OpenAI Gym  convention
    }
    )
  )

