Performance = R6Class("Performance",
  public = list(
    list.reward.epi = NULL,
    list.rewardPerEpisode = NULL,
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
      print(self$list.reward.epi)
      #print(self$epi.idx)
      #print(self$list.rewardPerEpisode)
      #print(self$list.discountedRPerEpisode)
      print("steps per episode")
      print(self$list.stepsPerEpisode)
    },

    observe = function() {

    }
    ),
  private = list(),
  active = list())


