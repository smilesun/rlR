#' @title ReinforceWithBaseline
#'
#' @format \code{\link{R6Class}} object
#' @description ReinforceWithBaseline
#'
#' @return [\code{\link{AgentPGBaseline}}].
#' @export
AgentPGBaseline = R6::R6Class("AgentPGBaseline",
  inherit = AgentPG,
  public = list(
    brain_actor = NULL,  # cross entropy loss
    brain_critic = NULL, # mse loss
    critic_yhat = NULL,
    p.old.a = NULL,
    p.next.a = NULL,
    p.old.c = NULL,
    p.next.c = NULL,
    delta = NULL,
    list.rewards = NULL,
    initialize = function(env, conf) {
      super$initialize(env, conf = conf)
      self$setBrain()
    },

    setBrain = function() {
      # FIXME: do we really need to call super$setBrain?
      # super$setBrain()
      self$task = "policy_fun"
      self$brain_actor = SurroNN$new(self, arch_list_name = "agent.nn.arch.actor")
      self$task = "value_fun"
      self$brain_critic = SurroNN$new(self, arch_list_name = "agent.nn.arch.critic", act_cnt = 1L)
      self$model = self$brain_critic
    },

    makeCnn = function()  {
      if (self$task == "critic") return(makeCnnCritic(input_shape = self$stateDim, act_cnt = 1))
      if (self$task == "actor")  return(makeCnnActor(input_shape = self$stateDim, act_cnt = self$act_cnt))
    },

     getReplayYhat = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
        self$list.rewards = lapply(self$list.replay, ReplayMem$extractReward)
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        self$model = self$brain_critic
        self$p.old.c = self$getYhat(list.states.old)
        self$p.next.c = self$getYhat(list.states.next)
        temp = simplify2array(list.states.old) # R array put elements columnwise
        mdim = dim(temp)
        norder = length(mdim)
        self$replay.x = aperm(temp, c(norder, 1:(norder - 1)))
    },

     replay = function(batchsize) {
          self$getReplayYhat(batchsize)
          len = length(self$list.replay)   # replay.list might be smaller than batchsize
          self$delta = self$advantage - self$p.old.c
          vec.step = unlist(lapply(self$list.replay, ReplayMem$extractStep))
          ded = sapply(vec.step, function(x) cumprod(rep(self$gamma, x))[x])
          if (length(ded) > 1) {
            ded = ded - mean(ded)  #  normalize
            ded = ded / sqrt(sum(ded ^ 2))  # normalize
          }
          list.targets.actor = lapply(1:len, function(i) as.vector(self$extractActorTarget(i)))
          list.targets.actor = lapply(1:len, function(i) list.targets.actor[[i]] * ded[i])
          list.targets.critic = lapply(1:len, function(i) as.vector(self$extractCriticTarget(i)))
          y_actor = t(simplify2array(list.targets.actor))
          y_critic = array(unlist(list.targets.critic), dim = c(len, 1L))
          self$brain_actor$train(self$replay.x, y_actor)  # update the policy model
          self$brain_critic$train(self$replay.x, y_critic)  # update the policy model
      },

      extractCriticTarget = function(i) {
          y = self$p.old.c[i, ] + self$delta[i]
          return(y)
      },

      extractActorTarget = function(i) {
          act = self$list.acts[[i]]
          advantage = (+1.0) * as.vector(self$delta[i])
          #FIXME: interestingly, multiply advantage by -1 also works
          vec.act = rep(0L, self$act_cnt)
          vec.act[act] = 1.0
          target = advantage * array(vec.act, dim = c(1L, self$act_cnt))
          return(target)
    },

    adaptLearnRate = function() {
        self$brain_actor$lr =  self$brain_actor$lr * self$lr_decay
        self$brain_critic$lr =  self$brain_critic$lr * self$lr_decay
    },

    afterStep = function() {
        self$policy$afterStep()
    },

    afterEpisode = function(interact) {
        self$getAdv(interact)
        self$replay(self$interact$perf$total.step)
        self$policy$afterEpisode()
        self$mem$afterEpisode()
        if (self$flag_rescue) self$interact$perf$rescue()
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain_actor$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    }
    ) # public
)

rlR.conf.PGBaseline = function() {
  RLConf$new(
           render = FALSE,
           policy.name = "ProbEpsilon",
           policy.epsilon = 1,
           policy.minEpsilon = 0.01,
           policy.decay = exp(-0.001),
           replay.memname = "Latest",
           agent.nn.arch.actor = list(nhidden = 64, act1 = "tanh", act2 = "softmax", loss = "categorical_crossentropy", lr = 25e-3, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=0.0001)", decay = 0.9, clipnorm = 5),
          agent.nn.arch.critic = list(nhidden = 64, act1 = "tanh", act2 = "linear", loss = "mse", lr = 25e-3, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=0)", decay = 0.9, clipnorm = 5)
          )
}
