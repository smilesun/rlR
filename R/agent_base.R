#' @title Discrete Action
#'
#' @description Discrete Action
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentArmed = R6Class("AgentArmed",  # agent do choose between arms
  public = list(
    # constructor init
    advantage = NULL,
    list.acts = NULL,
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
    epochs = NULL,
    replay.size = NULL,
    # member function
    # constructor
    initialize = function(actCnt, stateCnt, conf) {
      self$random.cnt = 0L
      self$epi.idx = 1L
      self$actCnt = actCnt
      self$stateCnt = stateCnt
      self$conf = conf
      self$vec.arm.q = vector(mode = "numeric", length = self$actCnt)
      self$epsilon = self$conf$get("policy.epsilon")
      self$replay.size = self$conf$get("replay.batchsize")
      self$gamma = self$conf$get("agent.gamma")
      self$glogger = RLLog$new(conf)
      self$epochs = conf$get("replay.epochs")
      memname = conf$get("replay.memname")
      self$mem = ReplayMem$factory(memname)(agent = self, conf = conf)
      policy_fun = conf$get("policy.name")
      self$policy = PolicyFactory$make(policy_fun, self)
    },

    # transform observation to  the replay memory
    observe = function(state.old, action, reward, state.new, action.new = NULL, delta = NULL, context = NULL) {
      ins = self$mem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, delta = NULL, context = context)
      self$glogger$log.nn$info("sars_delta: %s", ReplayMem$ins2String(ins))
      self$mem$add(ins)
    },

    calculateTDError = function(ins) {
      vec.mt = self$extractTarget(ins)
      err = vec.mt - self$yhat
      mean(err ^ 2)
    },

    updateDT = function(x, y) {
      yhat = self$brain$pred(x)
      self$mem$updateDT(yhat, y)
    },

    extractTarget = function(ins) {
      stop("not implemented")
    },

    setAdvantage = function(adv) {
      self$advantage = adv
    },

    replay = function(batchsize) {
        list.x.y = self$getXY(batchsize)
        x = list.x.y$x
        y = list.x.y$y
        self$brain$train(x, y, self$epochs)  # update the policy model
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    },

    getXY = function(batchsize) {
        list.res = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        #for (i in self$mem$replayed.idx) {
        #  self$glogger$log.nn$info("%s", self$mem$samples[[i]])
        #}
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
    },

    sampleRandomAct = function(state) {
        self$random.action = self$randomAct
    },

    afterStep = function() {
      # do nothing
    },

    decayEpsilon = function() {
      temp = self$epsilon * self$conf$get("policy.decay")
      self$epsilon = max(temp, self$conf$get("policy.minEpsilon"))
      cat(sprintf("Epsilon%f \n", temp))  # same message to console
      self$glogger$log.nn$info("rand steps:%i \n", self$random.cnt)
      cat(sprintf("rand steps:%i \n", self$random.cnt))  # same message to console
      self$random.cnt = 0L
    },

    afterEpisode = function() {
      # do nothing
    }
    ), # public
  private = list(),
  active = list(
    randomAct = function() {
      sample.int(self$actCnt)[1L] - 1L  # OpenAI Gym  convention
    }
    )
  )
