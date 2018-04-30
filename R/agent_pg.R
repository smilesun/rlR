#' @title
#'
#' @description
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentPG = R6Class("AgentPG",
  inherit = AgentArmed,
  public = list(
    advantage = NULL,
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
      self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$makeNN4PG)
},

    # extract target from one instance of replay memory, which is the one hot encoded action multiplied by the advantage of this episode
    extractTarget = function(ins) {
        act =  ReplayMem$extractAction(ins)
        temp.act = rep(0L, self$actCnt)
        temp.act[act + 1L] =  1L
        label = array(temp.act, dim = c(1L, self$actCnt))
        mt = label * self$advantage * (-1) # 'loss' maximization
        return(mt)
    },

    afterEpisode = function(interact) {
        episode.idx = interact$perf$epi.idx
        total.reward = sum(interact$perf$list.reward.epi[[episode.idx]])
        total.step = unlist(interact$perf$list.stepsPerEpisode)[episode.idx]
        adg = total.reward / total.step
        self$setAdvantage(adg)
        self$replay(total.step)   # key difference here
    }
    ), # public
  private = list(),
  active = list(
    )
  )
