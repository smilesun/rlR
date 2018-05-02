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
    extractTarget = function(ins) {
        act =  ReplayMem$extractAction(ins)
        temp.act = rep(0L, self$actCnt)
        temp.act[act + 1L] =  1L
        label = array(temp.act, dim = c(1L, self$actCnt))
        return(label)
    },

    replay = function(batchsize) {
        list.x.y = self$getXY(batchsize)
        x = list.x.y$x
        y = list.x.y$y
        y = y * self$advantage * (+1)
        self$brain$train(x, y, self$epochs)  # update the policy model
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
        if (self$conf$get("policy.name") == "policy.epsilonGreedy") self$decayEpsilon()
    }
    ), # public
  private = list(),
  active = list(
    )
  )
