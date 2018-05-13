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
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
      self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$dqn, conf$get("agent.nn.arch"))
},

    # extract target from one instance of replay memory, which is the one hot encoded action multiplied by the advantage of this episode
    extractTarget = function(i) {
        ins = self$list.replay[[i]]
        act =  ReplayMem$extractAction(ins)
        temp.act = rep(0L, self$actCnt)
        temp.act[act] =  1L
        label = temp.act
        #label = array(temp.act, dim = c(1L, self$actCnt))
        return(label)
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
        self$replay.x = as.array(t(as.data.table(list.states.old)))  # array put elements columnwise
        self$replay.y = as.array(t(as.data.table(list.targets)))  # array put elements columnwise
    },

    replay = function(batchsize) {
        self$getXY(batchsize)
        self$replay.y = self$replay.y * self$advantage * (+1)
        self$brain$train(self$replay.x, self$replay.y, self$epochs)  # update the policy model
    },

    afterEpisode = function(interact) {
        episode.idx = interact$perf$epi.idx
        total.reward = sum(interact$perf$list.reward.epi[[episode.idx]])
        total.step = unlist(interact$perf$list.stepsPerEpisode)[episode.idx]
        adg = interact$perf$list.discount.reward.epi[[episode.idx]]
        adg = adg - mean(adg)
        adg = adg / sum(adg ^ 2)
        self$setAdvantage(adg)
        self$replay(total.step)   # key difference here

    }
    ), # public
  private = list(),
  active = list(
    )
  )

pg = function(iter = 5000L, name = "CartPole-v0") {
  conf = rlR::RLConf$new(
           policy.epsilon = 1,
           policy.decay = exp(-0.001),
           policy.minEpsilon = 0.01,
           policy.name = "EpsilonGreedy",
           replay.memname = "Latest",
           replay.epochs = 1L,
           agent.nn.arch = list(nhidden = 64, act1 = "sigmoid", act2 = "softmax", loss = "categorical_crossentropy", lr = 5e-5, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0)"))
  interact = rlR::makeGymExperiment(sname = name, aname = "AgentPG", conf = conf)
  perf = interact$run(iter)
  return(perf)
}
