#' @title AgentActorCritic
#'
#' @description
#'
#' @param "AgentActorCritic" value
#' @param inherit value
#' @param public value
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentActorCritic = R6Class("AgentActorCritic",
  inherit = AgentPGBaseline,
  public = list(
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt, stateCnt, conf = conf)
    },

    replay = function(batchsize) {
              self$getReplayYhat(batchsize)
              len = length(self$list.replay)
              nv = self$gamma * self$p.next.c
              vec.done = unlist(lapply(self$list.replay, ReplayMem$extractDone))
              idx = which(vec.done)
              nv[idx, ] = 0   # at episode end, v[next] = 0
              self$delta = (unlist(self$list.rewards) + nv) - self$p.old.c  # Bellman Error as advantage
              ded = cumprod(rep(self$gamma, len))   # FIXME: this restain the agent to do uniform replay
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
        self$getAdv(interact)
        self$replay(self$total.step)
        self$policy$afterEpisode()
        self$mem$afterEpisode()
    }

    )
  )

ac_cart_pole = function(iter = 200L) {
  conf = rlR::RLConf$new(
           policy.name = "ProbEpsilon",
           policy.epsilon = 1,
           policy.minEpsilon = 0.01,
           policy.decay = exp(-0.001),
           replay.memname = "Latest",
           replay.batchsize = 64,
           agent.nn.arch.actor = list(nhidden = 64, act1 = "tanh", act2 = "softmax", loss = "categorical_crossentropy", lr = 25e-3, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=0.0001)", decay = 0.9, clipnorm = 5),
          agent.nn.arch.critic = list(nhidden = 64, act1 = "tanh", act2 = "linear", loss = "mse", lr = 25e-3, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=0)", decay = 0.9, clipnorm = 5)
          )
  interact = rlR::makeGymExperiment(sname = "CartPole-v0", "AgentActorCritic", conf = conf)
  perf = interact$run(iter)
  return(perf)
}
