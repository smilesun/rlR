#' @title Discrete Action Agent
#'
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link{R6Class}} to represent Discrete Action Agent or Armed Agent
#'
#' @section Member Variables:
#'
#' \describe{
#'   \item{interact}{[\code{Interaction}] \cr
#'   Interaction object between Agent and Environment.}
#'   \item{mem}{[\code{ReplayMem}] \cr
#'     Replay memory for the Agent}
#'   \item{brain}{[\code{Surrogate}] \cr
#'     A Surrogate model to evaluate the current state.}
#'   \item{model}{[\code{Surrogate}] \cr
#'     A reference to the Surrogate model.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{updatePara(name, val)}{[\code{function}] \cr Function to update parameter setting.}
#'   \item{continue(new_env, iter)}{[\code{function}] \cr Continue with a new environment new_env for iter number of episodes}
#'   \item{learn(iter)}{[\code{function}] \cr Run iter number of Episodes}
#' }
#' @return [\code{\link{AgentArmed}}].
#' @export
AgentArmed = R6::R6Class("AgentArmed",
  public = list(
    # constructor init
    interact = NULL,
    mem = NULL,  # replay memory
    advantage = NULL,
    list.acts = NULL,
    random.cnt = NULL,
    actCnt = NULL,
    stateCnt = NULL,
    stateDim = NULL,
    conf = NULL,
    vec.arm.q = NULL,      # store Q value for each arm
    random.action = NULL,  # store random.action
    # built from conf
    glogger = NULL,
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
    env = NULL,
    # member function
    # constructor
    initialize = function(env, conf) {
      self$random.cnt = 0L
      self$initializeEnv(env)
      self$initializeConf(conf = conf)
    },

    initializeEnv = function(env) {
      self$env = env
      self$actCnt =  env$act_cnt
      self$stateDim = env$state_dim
      self$vec.arm.q = vector(mode = "numeric", length = self$actCnt)
    },

    # seperate initializeConf allow for reconfiguration
    initializeConf = function(conf = NULL) {
      self$conf = conf
      if (!is.null(self$conf)) {
        self$buildConf()
      }
    },

    setConf = function(conf) {
      self$conf = conf
      self$buildConf()
    },

    updatePara = function(name, val) {
      self$conf$updatePara(name, val)
      self$buildConf()
    },

    loginfo = function(str, ...) {
      self$glogger$log.nn$info(str, ...)
    },

    createInteract = function(rl.env) {
      self$interact = Interaction$new(rl.env = rl.env, rl.agent = self)
    },

    buildConf = function() {
      self$replay.size = self$conf$get("replay.batchsize")
      self$gamma = self$conf$get("agent.gamma")
      self$epochs = self$conf$get("replay.epochs")
      # object
      memname = self$conf$get("replay.memname")
      self$mem = ReplayMem$factory(memname, agent = self, conf = self$conf)
      policy_fun = self$conf$get("policy.name")
      self$policy = makePolicy(policy_fun, self)
      self$glogger = RLLog$new(self$conf)
      self$createInteract(self$env)  # initialize after all other members are initialized!!
      self$setBrain()
    },

    setBrain = function() {
      # do nothing
    },

    # transform observation to  the replay memory
    observe = function(state.old, action, reward, state.new, done, info, episode, stepidx, action.new = NULL) {
      ins = self$mem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = list(episode = episode, stepidx = stepidx, info = info))
      self$glogger$log.nn$info("sars_delta: %s", ReplayMem$ins2String(ins))
      self$mem$add(ins)
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
      p = dim(list.states.old[[1]])
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
        temp = Reduce(rbind, list.states.old)
        nr = length(list.states.old)
        temp = simplify2array(list.states.old) # R array put elements columnwise
        mdim = dim(temp)
        norder = length(mdim)
        self$replay.x = aperm(temp, c(norder, 1:(norder - 1)))
        # assert(self$replay.x[1,,,]== list.states.old[[1L]])
        self$replay.y = as.array(t(as.data.table(list.targets)))  # array put elements columnwise
    },

    act = function(state) {
      assert(class(state) == "array")
      self$evaluateArm(state)  # calculation will be used for the policy to decide which arm to use
      act = self$policy$act(state)  # returning the chosen action
      return(act)
    },

    sampleRandomAct = function(state) {
        self$random.action = sample.int(self$actCnt)[1L]
    },

    afterStep = function() {
      # do nothing
    },

    afterEpisode = function() {
      self$policy$afterEpisode()
      self$mem$afterEpisode()
    },

    learn = function(iter) {
      self$interact$run(iter)
    },

    continue = function(new_env, iter) {
      self$mem$reset()  ##clean memory
      self$interact = Interaction$new(rl.env = new_env, rl.agent = self)
      self$interact$run(maxiter = iter)
    }
    ), # public
  private = list(),
  active = list()
  )
