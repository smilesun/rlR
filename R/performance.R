Performance = R6Class("Performance",
  public = list(
    list.reward.epi = NULL,  # take reward vector of each episode
    list.rewardPerEpisode = NULL,  # sum up reward of each episode
    rewardPerStep = NULL,
    list.discountedRPerEpisode = NULL,
    list.stepsPerEpisode = NULL,
    epi.idx = NULL,
    glogger = NULL,

    initialize = function(glogger) {
      self$glogger = glogger
      self$list.reward.epi = list()
      self$epi.idx = 0L
      self$list.rewardPerEpisode = list()
      self$list.discountedRPerEpisode = list()
      self$list.stepsPerEpisode = list()
    },

    toString = function() {
      s1 = sprintf("steps per episode:%s \n", toString(self$list.stepsPerEpisode))
      self$list.rewardPerEpisode = lapply(self$list.reward.epi, function(x) sum(x))
      s2 = sprintf("total reward per episode: %s \n", toString(self$list.rewardPerEpisode))
      self$rewardPerStep = unlist(self$list.rewardPerEpisode) / unlist(self$list.stepsPerEpisode)
      s3 = sprintf("reward per step per episode:%s \n", toString(self$rewardPerStep))
      paste(s1, s2, s3)
    },

    observe = function() {

    }
    ),
  private = list(),
  active = list())

