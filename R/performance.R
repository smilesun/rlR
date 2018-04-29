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

    persist = function(path) {
      perf = self$clone()
      save(perf, file = path)
    },

    toString = function() {
      s1 = sprintf("steps per episode:%s \n", toString(self$list.stepsPerEpisode))
      self$list.rewardPerEpisode = lapply(self$list.reward.epi, function(x) sum(x))
      s2 = sprintf("total reward per episode: %s \n", toString(self$list.rewardPerEpisode))
      self$rewardPerStep = unlist(self$list.rewardPerEpisode) / unlist(self$list.stepsPerEpisode)
      s3 = sprintf("reward per step per episode:%s \n", toString(self$rewardPerStep))
      paste(s1, s2, s3)
    },

    plot = function() {
      library(ggplot2)
      se = unlist(self$list.stepsPerEpisode)
      df = data.frame(episode = seq_along(se),
        steps = se)
      ggplot(df, aes(episode, steps), col = "brown1") +
        geom_point(alpha = 0.2) +
        theme_bw() +
        labs(
          title = "Steps Per Episode",
          x = "Episode",
          y = "Steps per episode"
          ) +
        coord_cartesian(ylim = c(0, 200)) +
        geom_smooth(se = FALSE, size = 1) +
        geom_hline(yintercept = 15, size = 1, col = "black", lty = 2)
    },

    toScalar = function() {
       n = length(self$list.rewardPerEpisode)
       res = unlist(self$list.rewardPerEpisode)
       res[n]
    }
    ),
  private = list(),
  active = list())

