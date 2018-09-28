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
    flag_rescue = NULL,
    initialize = function(env, conf) {
      self$flag_rescue = conf$get("agent.flag.reset.net")
      super$initialize(env, conf = conf)
      self$setBrain()
    },

    setBrain = function() {
      self$task = "policy_fun"
      self$brain = SurroNN$new(self, arch_list_name = "agent.nn.arch")
      self$model = self$brain
    },

    extractTarget = function(ins) {
        act =  ReplayMem$extractAction(ins)
        temp.act = rep(0.0, self$act_cnt)
        temp.act[act] =  1.0
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
        temp = simplify2array(list.states.old)
        mdim = dim(temp)
        norder = length(mdim)
        self$replay.x = aperm(temp, c(norder, 1:(norder - 1)))
        self$replay.y = t(simplify2array(list.targets))
    },

    replay = function(batchsize) {
        self$getXY(batchsize)
        self$replay.x = array_reshape(self$replay.x, c(batchsize, self$state_dim))
        self$replay.y = self$replay.y * self$advantage * (+1)  # elementwise multiplication
        self$brain$train(self$replay.x, self$replay.y, self$epochs)  # update the policy model
    },

    getAdv = function(interact) {
        episode.idx = interact$perf$epi_idx
        total.reward = sum(interact$perf$list.reward.epi[[episode.idx]])
        self$interact$perf$total.step = unlist(interact$perf$list.stepsPerEpisode)[episode.idx]
        adg = interact$perf$list.discount.reward.epi[[episode.idx]]
        if (length(adg) > 1) {
          adg = adg - mean(adg)
          adg = adg / sqrt(sum(adg ^ 2))
        }
        self$setAdvantage(adg)
    },

    afterStep = function() {
        self$policy$afterStep()
    },

    afterEpisode = function(interact) {
        self$getAdv(interact)  # total.step is calculated here
        self$replay(self$interact$perf$total.step)   # key difference here
        self$policy$afterEpisode()
        if (self$flag_rescue) self$interact$perf$rescue()
    }
    ) # public
)
