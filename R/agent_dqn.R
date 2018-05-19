#' @title  DQN
#' 
#' @description DQN
#' 
#' @param "AgentDQN" value
#' @param inherit value
#' @param public value
#' @param stateCnt value
#' @param conf value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
AgentDQN = R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    initialize = function(actCnt, stateCnt, conf = NULL) {
       if (is.null(conf)) conf = rlR.conf.DQN()
       super$initialize(actCnt, stateCnt, conf)
       self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, arch.list = conf$get("agent.nn.arch"))
       self$model = self$brain
    },

    extractTarget = function(i) {
        ins = self$list.replay[[i]]
        act2update =  ReplayMem$extractAction(ins)
        p.old = self$p.old[i, ]
        self$yhat = p.old  # for calculating the  TD error
        r = ReplayMem$extractReward(ins)
        done = ReplayMem$extractDone(ins)
        if (done) {
          target = r
        } else {
          vec.next.Q = self$p.next[i, ]
          a_1 = which.max(vec.next.Q)  # action index start from 1L
          target = r + self$gamma * max(vec.next.Q)
        }
        mt = p.old
        mt[act2update] = target  # the not active action arm's Q will not be updated
        return(mt)
    },

    afterStep = function() {
          self$replay(self$replay.size)
          self$policy$afterStep()
    },

    afterEpisode = function(interact) {
          self$policy$afterEpisode()
          self$mem$afterEpisode()
    }
    ), # public
  private = list(),
  active = list(
    )
  )

rlR.conf.DQN = function() {
  rlR::RLConf$new(render = TRUE,
          policy.maxEpsilon = 1,
          policy.decay = exp(-0.001),
          policy.name = "EpsilonGreedy",
          replay.batchsize = 64L,
          agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))
}

AgentDQN$test = function(iter = 2000L, sname = "CartPole-v0", render = FALSE) {
  interact = makeGymExperiment(sname = sname, aname = "AgentDQN", conf = rlR.conf.DQN())
  perf = interact$run(iter)
  return(perf)
}
