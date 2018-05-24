#' @title AgentActorCritic
#'
#' @format \code{\link{R6Class}} object
#' @description ActorCritic Agent
#'
#' @section Methods:
#' Inherited from \code{AgentArmed}:
#' @inheritSection AgentArmed Methods
#'
#' @return [\code{\link{AgentActorCritic}}].
#' @export
AgentActorCritic = R6::R6Class("AgentActorCritic",
  inherit = AgentPGBaseline,
  public = list(
    initialize = function(env, conf = NULL) {
      if (is.null(conf)) conf = rlR.conf.AC()
      super$initialize(env, conf = conf)
    },

    # update para is already defined in parent class

    replay = function(batchsize) {
      self$getReplayYhat(batchsize)
      len = length(self$list.replay)
      nv = self$gamma * self$p.next.c
      vec.done = unlist(lapply(self$list.replay, ReplayMem$extractDone))
      idx = which(vec.done)
      nv[idx, ] = 0   # at episode end, v[next] = 0
      self$delta = (unlist(self$list.rewards) + nv) - self$p.old.c  # Bellman Error as advantage
      self$interact$toConsole("totoal delta: %s", sum(self$delta) / length(self$delta))
      vec.step = unlist(lapply(self$list.replay, ReplayMem$extractStep))
      ded = sapply(vec.step, function(x) cumprod(rep(self$gamma, x))[x])
      ded = ded - mean(ded)  #  normalize
      ded = ded / sum(ded ^ 2)  # normalize
      list.targets.actor = lapply(1:len, function(i) as.vector(self$extractActorTarget(i)))
      list.targets.actor = lapply(1:len, function(i) list.targets.actor[[i]] * ded[i])
      list.targets.critic = lapply(1:len, function(i) as.vector(self$extractCriticTarget(i)))
      y_actor = t(simplify2array(list.targets.actor))
      y_critic = array(unlist(list.targets.critic), dim = c(len, 1L))
      self$brain_critic$train(self$replay.x, y_critic)  # first update critic
      self$brain_actor$train(self$replay.x, y_actor)
    },

      extractCriticTarget = function(i) {
          y = self$p.old.c[i, ] + self$delta[i]
          return(y)
      },

      extractActorTarget = function(i) {
          advantage = self$delta[i]
          act = self$list.acts[[i]]
          advantage = (+1) * as.vector(advantage)
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
    }

  )
)


rlR.conf.AC = function() {
  conf = RLConf$new(
           render = TRUE,
           log = FALSE,
           console = FALSE,
           policy.name = "EpsilonGreedy",
           policy.maxEpsilon = 1,
           policy.minEpsilon = 0.001,
           policy.decay = exp(-0.006),
           replay.epochs = 1L,
           replay.memname = "Latest",
           agent.nn.arch.actor = list(nhidden = 64, act1 = "tanh", act2 = "softmax", loss = "categorical_crossentropy", lr = 1e-4, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=1e-4)", decay = 0.9, clipnorm = 5),
        agent.nn.arch.critic = list(nhidden = 64, act1 = "tanh", act2 = "linear", loss = "mse", lr = 1e-4, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=1e-4)", decay = 0.9, clipnorm = 5)
          )
}

AgentActorCritic$test = function(iter = 1000L, sname = "CartPole-v0", render = TRUE) {
  conf = rlR.conf.AC()
  conf$updatePara("console", TRUE)
  conf$updatePara("render", render)
  interact = makeGymExperiment(sname = sname, "AgentActorCritic", conf)
  perf = interact$run(iter)
  return(perf)
}
