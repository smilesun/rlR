#' @title Policy Gradient
#'
#' @description Policy Gradient
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentPG = R6Class("AgentPG",
  inherit = AgentArmed,
  public = list(
    total.step = NULL,
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
      self$brain = SurroNN4PG$new(actCnt = self$actCnt, stateCnt = self$stateCnt, arch.list = conf$get("agent.nn.arch"))
},
    extractTarget = function(ins) {
        act =  ReplayMem$extractAction(ins)
        temp.act = rep(0L, self$actCnt)
        temp.act[act] =  1L
        return(temp.act)
    },

    getXY = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        len = length(self$list.replay)
        ded = cumprod(rep(self$gamma, len))   # FIXME: this restain the agent to do uniform replay
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.targets = lapply(self$list.replay, self$extractTarget)
        list.targets = lapply(1:len, function(i) ded[i] * list.targets[[i]])
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        self$replay.x = as.array(t(as.data.table(list.states.old)))  # array put elements columnwise
        self$replay.y = as.array(t(as.data.table(list.targets)))  # array put elements columnwise
    },

    replay = function(batchsize) {
        self$getXY(batchsize)
        self$replay.y = self$replay.y * self$advantage * (-1)
        self$brain$train(self$replay.x, self$replay.y, self$epochs)  # update the policy model
    },

    getAdv = function(interact) {
        episode.idx = interact$perf$epi.idx
        total.reward = sum(interact$perf$list.reward.epi[[episode.idx]])
        self$total.step = unlist(interact$perf$list.stepsPerEpisode)[episode.idx]
        adg = interact$perf$list.discount.reward.epi[[episode.idx]]
        adg = adg - mean(adg)
        adg = adg / sum(adg ^ 2)
        self$setAdvantage(adg)
    },

    afterStep = function() {
        self$policy$afterStep()
    },

    afterEpisode = function(interact) {
        self$getAdv(interact)
        self$replay(self$total.step)   # key difference here
        self$policy$afterEpisode()
    }
    ), # public
  private = list(),
  active = list(
    )
  )

AgentPG$test = function(iter = 5000L, sname = "CartPole-v0", render = TRUE) {
  conf = rlR::RLConf$new(
           render = render,
           policy.epsilon = 1,
           policy.decay = exp(-0.001),
           policy.minEpsilon = 0.01,
           policy.name = "ProbEpsilon",
           replay.memname = "Latest",
           replay.epochs = 1L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "softmax", loss = "categorical_crossentropy", lr = 25e-3, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0)"))
  interact = rlR::makeGymExperiment(sname = sname, aname = "AgentPG", conf = conf)
  perf = interact$run(iter)
  return(perf)
}
