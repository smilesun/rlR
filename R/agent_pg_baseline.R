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
      if (is.null(conf)) conf = rlR.AgentPGBaseline.conf()
      super$initialize(env, conf = conf)
      self$setBrain()
      },

    setBrain = function() {
      super$setBrain()
      self$brain_actor = SurroNN4PG$new(actCnt = self$actCnt, stateDim = self$stateDim, arch.list = self$conf$get("agent.nn.arch.actor"))
      self$brain_critic = SurroNN4PG$new(actCnt = 1L, stateDim = self$stateDim, arch.list = self$conf$get("agent.nn.arch.critic"))
    },

     getReplayYhat = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
        self$list.rewards = lapply(self$list.replay, ReplayMem$extractReward)
        self$model = self$brain_critic
        self$p.old.c = self$getYhat(list.states.old)
        self$p.next.c = self$getYhat(list.states.next)
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        self$replay.x = as.array(t(as.data.table(list.states.old)))  # array put elements columnwise
    },

     replay = function(batchsize) {
          self$getReplayYhat(batchsize)
          len = length(self$list.replay)   # replay.list might be smaller than batchsize
          self$delta = self$advantage - self$p.old.c
          vec.step = unlist(lapply(self$list.replay, ReplayMem$extractStep))
          ded = sapply(vec.step, function(x) cumprod(rep(self$gamma, x))[x])
          ded = ded - mean(ded)  #  normalize
          ded = ded / sum(ded ^ 2)  # normalize
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
          ins = self$list.replay[[i]]
          act = ReplayMem$extractAction(ins)
          advantage = (+1) * as.vector(self$delta[i])
          #FIXME: interestingly, multiply advantage by -1 also works
          vec.act = rep(0L, self$actCnt)
          vec.act[act] = 1L
          target = advantage * array(vec.act, dim = c(1L, self$actCnt))
          return(target)
    },

    afterStep = function() {
        self$policy$afterStep()
    },

    afterEpisode = function(interact) {
        self$getAdv(interact)
        self$replay(self$total.step)
        self$policy$afterEpisode()
        self$mem$afterEpisode()
        self$rescue()
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain_actor$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    }
    ), # public
  private = list(),
  active = list(
    )
  )

rlR.AgentPGBaseline.conf = function() {
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

AgentPGBaseline$test = function(iter = 1000L, sname = "CartPole-v0", render = TRUE) {
  interact = makeGymExperiment(sname = "CartPole-v0", "AgentPGBaseline", conf = rlR.AgentPGBaseline.conf())
  perf = interact$run(iter)
  return(perf)
}
