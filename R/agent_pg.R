#' @title Policy Gradient
#'
#' @format \code{\link{R6Class}} object
#' @description Policy Gradient
#'
#' @section Methods:
#' Inherited from \code{AgentArmed}:
#' @inheritSection AgentArmed Methods
#'
#' @return [\code{\link{AgentPG}}].
#' @export
AgentPG = R6::R6Class("AgentPG",
  inherit = AgentArmed,
  public = list(
    total.step = NULL,
    initialize = function(env, conf) {
      if (is.null(conf)) conf = rlR.AgentPG.conf()
      super$initialize(env, conf = conf)
      self$brain = SurroNN4PG$new(actCnt = self$actCnt, stateDim = self$stateDim, arch.list = self$conf$get("agent.nn.arch"))
},
    updatePara = function(name, val) {
          super$updatePara(name, val)
          self$brain = SurroNN4PG$new(actCnt = self$actCnt, stateDim = self$stateDim, arch.list = self$conf$get("agent.nn.arch"))
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
        vec.step = unlist(lapply(self$list.replay, ReplayMem$extractStep))
        ded = sapply(vec.step, function(x) cumprod(rep(self$gamma, x))[x])
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

rlR.AgentPG.conf = function() {
  RLConf$new(
          render = FALSE,
          policy.maxEpsilon = 1,
          policy.decay = exp(-0.001),
          policy.minEpsilon = 0.01,
          policy.name = "ProbEpsilon",
          replay.memname = "Latest",
          replay.epochs = 1L,
          agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "softmax", loss = "categorical_crossentropy", lr = 25e-3, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0)"))
}

AgentPG$test = function(iter = 1000L, sname = "CartPole-v0", render = TRUE) {
  interact = makeGymExperiment(sname = sname, aname = "AgentPG", conf = rlR.AgentPG.conf())
  perf = interact$run(iter)
  return(perf)
}
