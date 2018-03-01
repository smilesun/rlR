Performance = R6Class("Performance",
  public = list(
    list.reward.epi = NULL,  # take reward vector of each episode
    list.rewardPerEpisode = NULL,  # sum up reward of each episode
    rewardPerStep = NULL,
    list.discountedRPerEpisode = NULL,
    list.stepsPerEpisode = NULL,
    epi.idx = NULL,

    initialize = function() {
      self$list.reward.epi = list()
      self$epi.idx = 0L
      self$list.rewardPerEpisode = list()
      self$list.discountedRPerEpisode = list()
      self$list.stepsPerEpisode = list()
    },

    toString = function() {
      self$list.rewardPerEpisode = reward.perEpisode = lapply(self$list.reward.epi, function(x) sum(x))
      self$rewardPerStep = unlist(self$list.rewardPerEpisode)/unlist(self$list.stepsPerEpisode)
      print("steps per episode:")
      print(self$list.stepsPerEpisode)
    },

    observe = function() {

    }
    ),
  private = list(),
  active = list())


