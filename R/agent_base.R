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
    epi.idx = NULL,
    advantage = NULL,
    list.acts = NULL,
    random.cnt = NULL,
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
    model = NULL,
    yhat = NULL,  # bellman equation estimation
    epochs = NULL,
    replay.size = NULL,
    p.old = NULL,
    p.next = NULL,
    list.replay = NULL,
    replay.y = NULL,
    replay.x = NULL,
    gstep.idx = NULL,
    # member function
    # constructor
    initialize = function(actCnt, stateCnt, conf) {
      self$epi.idx = 1L
      self$random.cnt = 0L
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
      self$mem = ReplayMem$factory(memname, agent = self, conf = conf)
      policy_fun = conf$get("policy.name")
      self$policy = PolicyFactory$make(policy_fun, self)
      self$gstep.idx = 1L
    },

    # transform observation to  the replay memory
    observe = function(state.old, action, reward, state.new, done, info, episode, stepidx, action.new = NULL) {
      ins = self$mem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = list(episode = episode, stepidx = stepidx, info = info))
      self$glogger$log.nn$info("sars_delta: %s", ReplayMem$ins2String(ins))
      self$mem$add(ins)
      self$gstep.idx = self$gstep.idx + 1L
    },

    calculateTDError = function(ins) {
      vec.mt = self$extractTarget(ins)  # self$yhat is calculated inside err = vec.mt - self$yhat mean(err ^ 2)
    },

    extractTarget = function(ins) {
      stop("not implemented")
    },

    setAdvantage = function(adv) {
      self$advantage = adv
    },

    replay = function(batchsize) {
        self$getXY(batchsize)
        self$brain$train(self$replay.x, self$replay.y, self$epochs)  # update the policy model
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    },

    getYhat = function(list.states.old) {
      nr = length(list.states.old)
      p = length(list.states.old[[1]])
      old.state = Reduce(rbind, list.states.old)
      old.state = array(old.state, dim = c(nr, p))
      p.old = self$model$pred(old.state)
      return(p.old)
    },

    getXY = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
        self$p.old = self$getYhat(list.states.old)
        self$p.next = self$getYhat(list.states.next)
        list.targets = lapply(1:length(self$list.replay), self$extractTarget)
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        self$replay.x = as.array(t(as.data.table(list.states.old)))  # array put elements columnwise
        self$replay.y = as.array(t(as.data.table(list.targets)))  # array put elements columnwise
    },

    act = function(state) {
      assert(class(state) == "array")
      self$evaluateArm(state)  # calculation will be used for the policy to decide which arm to use
      act = self$policy$act(state)  # returning the chosen action
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
      self$policy$afterEpisode()
      self$mem$afterEpisode()
    },
    learn = function(iter) {
      interact = Interaction$new(rl.env = probe$env, rl.agent = rl.agent)
      Interact$run(iter)
    }
    ), # public
  private = list(),
  active = list(
    randomAct = function() {
      sample.int(self$actCnt)[1L]
    }
    )
  )
