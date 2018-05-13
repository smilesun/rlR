#' @title Actor Critic Agent
#'
#' @description Actor Critic Agent
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentActorCritic = R6Class("AgentActorCritic",
  inherit = AgentPG,
  public = list(
    brain_actor = NULL,  # cross entropy loss
    brain_critic = NULL, # mse loss
    critic_yhat = NULL,
    p.old.a = NULL,
    p.next.a = NULL,
    p.old.c = NULL,
    p.next.c = NULL,
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt, stateCnt, conf = conf)
      self$brain_actor = SurroNN4PG$new(actCnt = self$actCnt, stateCnt = self$stateCnt, arch.list = conf$get("agent.nn.arch.actor"))
      self$brain_critic = SurroNN4PG$new(actCnt = 1L, stateCnt = self$stateCnt, arch.list = conf$get("agent.nn.arch.critic"))
      },

     getReplayYhat = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
        self$model =  self$brain_actor
        self$p.old.a = self$getYhat(list.states.old)
        self$p.next.a = self$getYhat(list.states.next)
        self$model = self$brain_critic
        self$p.old.c = self$getYhat(list.states.old)
        self$p.next.c = self$getYhat(list.states.next)
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        self$replay.x = as.array(t(as.data.table(list.states.old)))  # array put elements columnwise
    },

     replay = function(batchsize) {
          self$getReplayYhat(batchsize)
          ded = cumprod(rep(self$gamma, batchsize))
          list.targets.actor = lapply(1:batchsize, function(i) as.vector(self$extractActorTarget(i)))
          list.targets.actor = lapply(1:batchsize, function(i) list.targets.actor[[i]] * ded[i])
          list.targets.critic = lapply(1:batchsize, function(i) as.vector(self$extractCriticTarget(i)))
          y_actor = t(simplify2array(list.targets.actor))
          y_critic = array(unlist(list.targets.critic), dim = c(batchsize, 1L))
          self$brain_actor$train(self$replay.x, y_actor)  # update the policy model
          self$brain_critic$train(self$replay.x, y_critic)  # update the policy model
      },

      extractCriticTarget = function(i) {
          ins = self$list.replay[[i]]
          r = ReplayMem$extractReward(ins)
          done = ReplayMem$extractDone(ins)
          if (done) {
            y = r
          } else {
            self$critic_yhat = self$p.old.c[i, ]
            y = r + self$gamma * self$p.next.c[i, ]
          }
          return(y)
      },

      extractActorTarget = function(i) {
          ins = self$list.replay[[i]]
          act = ReplayMem$extractAction(ins)
          critic.old.v = self$p.old.c[i, ]
          r = ReplayMem$extractReward(ins)
          done = ReplayMem$extractDone(ins)
          if (done) {
            advantage = r - critic.old.v   # always occur at the last step
          } else {
            critic.next.v = self$p.next.c[i, ]
            advantage = (r + self$gamma * critic.next.v) - critic.old.v  # Bellman Error as advantage
          }
          advantage = (+1) * as.vector(advantage)  # convert (1,1) matrix to scalar
          vec.act = rep(0L, self$actCnt)
          vec.act[act] = 1L
          target = advantage * array(vec.act, dim = c(1L, self$actCnt))
          return(target)
    },

    afterStep = function() {
        self$policy$afterStep()
    },

    afterEpisode = function(interact) {
        episode.idx = interact$perf$epi.idx
        total.step = unlist(interact$perf$list.stepsPerEpisode)[episode.idx]
        self$replay(total.step)
        self$policy$afterEpisode()
        self$mem$afterEpisode()
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

a3c_cart_pole = function(iter = 2000L) {
  conf = rlR::RLConf$new(
           policy.name = "EpsilonGreedy",
           policy.epsilon = 1,
           policy.minEpsilon = 0.01,
           policy.decay = exp(-0.001),
           replay.memname = "Latest",
           agent.nn.arch.actor = list(nhidden = 64, act1 = "tanh", act2 = "softmax", loss = "categorical_crossentropy", lr = 0.0001, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=0.0001)", decay = 0.9, clipnorm = 5),
          agent.nn.arch.critic = list(nhidden = 64, act1 = "tanh", act2 = "linear", loss = "mse", lr = 0.0001, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0001)")
           , decay = 0.9, clipnorm = 5)
  interact = rlR::makeGymExperiment(sname = "CartPole-v0", "AgentActorCritic", conf = conf)
  perf = interact$run(iter)
  return(perf)
}
