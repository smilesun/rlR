#' @title  DQN
#'
#' @format \code{\link{R6Class}} object
#' @description Deep Q Network
#'
#' @section Methods:
#' Inherited from \code{AgentArmed}:
#' @inheritSection AgentArmed Methods
#'
#' @return [\code{\link{AgentDQN}}].
AgentDQN = R6::R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    initialize = function(env, conf) {
       super$initialize(env, conf)
       self$setBrain()
    },

    setBrain = function() {
       self$task = "value_fun"
       self$brain = SurroNN$new(self)
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
          # equivalent to huber loss
          if (self$clip_td_err) {
            target = max(target, p.old[act2update] - 1L)
            target = min(target, p.old[act2update] + 1L)
          }
        }
        mt = p.old
        mt[act2update] = target  # the not active action arm's Q will not be updated
        #FIXME: shall here be 0?
        # mt[-act2update] = 0.0  # the not active action arm will be set to be zero
        return(mt)
    },

    afterStep = function() {
        if (self$interact$step_in_episode %% self$replay.freq == 0L) {
          self$replay(self$replay.size)
        }
        self$policy$afterStep()
    },

    afterEpisode = function(interact) {
          self$policy$afterEpisode()
          self$mem$afterEpisode()
          self$brain$afterEpisode()
    }
    ) # public
)
