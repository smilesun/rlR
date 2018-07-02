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
#'   \item{learn(iter)}{[\code{function}] \cr Run iter number of Episodes}
#' }
#' @return [\code{\link{AgentArmed}}].
#' @export
AgentArmed = R6::R6Class("AgentArmed",
  public = list(
    # constructor init
    task = NULL,  # string either critic or actor
    replay_delta = NULL,
    updateFreq = NULL,  # how often you should update the model
    lr_decay = NULL,
    interact = NULL,
    mem = NULL,  # replay memory
    advantage = NULL,
    list.acts = NULL,
    act_cnt = NULL,
    stateDim = NULL,
    conf = NULL,
    vec.arm.q = NULL,      # store Q value for each arm
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
    sess = NULL,
    replay.freq = NULL,
    network_build_funs = NULL,  # user specific function to create surrogate model
    # member function
    # constructor
    initialize = function(env, conf) {
      self$network_build_funs = vector(mode = "list", length = 2)
      names(self$network_build_funs) = c("policy_fun", "value_fun")
      self$sess = tensorflow::tf$Session()
      self$initializeEnv(env)
      self$initializeConf(conf = conf)
    },

    initializeEnv = function(env) {
      self$env = env
      self$act_cnt =  env$act_cnt
      self$stateDim = env$state_dim
      self$vec.arm.q = vector(mode = "numeric", length = self$act_cnt)
    },

    # user creation of brain from outside
    customizeBrain = function(policy_fun = NULL, value_fun = NULL) {
       if (!is.null(policy_fun)) {
          checkCustomNetwork(policy_fun, self$stateDim, self$act_cnt)
          self$network_build_funs[["policy_fun"]] = policy_fun
       }
       if (!is.null(value_fun)) {
          checkCustomNetwork(value_fun, self$stateDim, self$act_cnt)
          self$network_build_funs[["value_fun"]] = value_fun
       }
    },

    # seperate initializeConf allow for reconfiguration
    initializeConf = function(conf) {
      self$conf = conf
      self$buildConf()
    },

    setConf = function(conf) {
      self$conf = conf
      self$buildConf()
    },

    updatePara = function(...) {
      self$conf$set(...)
      self$buildConf()
      self$env$setAgent(self)   # update the observ_stack_len parameter
      self$setBrain()  # if the updated parameter ever changed the nn structure
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
      self$lr_decay = self$conf$get("agent.lr.decay")
      self$replay.freq = self$conf$get("replay.freq")
      # object
      memname = self$conf$get("replay.memname")
      self$mem = makeReplayMem(memname, agent = self, conf = self$conf)
      policy_name = self$conf$get("policy.name")
      self$policy = makePolicy(policy_name, self)
      self$glogger = RLLog$new(self$conf)
      self$createInteract(self$env)  # initialize after all other members are initialized!!
      self$setBrain()
    },

    # setBrain = function() {
    ## do nothing
    # },

    # transform observation to  the replay memory
    observe = function(interact) {
      state.old = interact$s.old
      action = interact$action
      reward = interact$s_r_done_info[[2L]]
      state.new = interact$s_r_done_info[[1L]]
      done = interact$s_r_done_info[[3L]]
      info = interact$s_r_done_info[[4]]
      episode = interact$idx.episode + 1L
      stepidx = interact$idx.step + 1L
      ins = self$mem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = list(episode = episode, stepidx = stepidx, info = info))
      self$mem$add(ins)
    },

    extractTarget = function(ins) {
      stop("not implemented")
    },

    setAdvantage = function(adv) {
      self$advantage = adv
    },

    replay = function(batchsize) {
      self$getXY(batchsize)
      self$glogger$log.nn$info("replaying average ythat vs target error %s", mean(self$replay_delta))
      self$model$train(self$replay.x, self$replay.y, self$epochs)  # update the policy model
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$model$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    },

    getYhat = function(list.states.old) {
      nr = length(list.states.old)
      p = dim(list.states.old[[1]])
      old.state = Reduce(rbind, list.states.old)
      #FIXME: The transform below works for CartPole-v0 and Pong-v0 but not continous environment
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
        self$replay.y = t(simplify2array(list.targets))  # array put elements columnwise
        diff_table = abs(self$replay.y - self$p.old)
        self$replay_delta = apply(diff_table, 1, mean)
    },

    act = function(state) {
      checkmate::assert_array(state)
      # in video, state could be stacked together with previous frame
      self$evaluateArm(state)  # calculation will be used for the policy to decide which arm to use
      act = self$policy$act(state)  # returning the chosen action
      return(act)
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
    }
  ) # public
)
    #'   \item{continue(new_env, iter)}{[\code{function}] \cr Continue with a new environment new_env for iter number of episodes}
    # continue = function(new_env, iter) {
    #   self$mem$reset()  ##clean memory
    #   self$interact = Interaction$new(rl.env = new_env, rl.agent = self)
    #   self$interact$run(maxiter = iter)
    # }

