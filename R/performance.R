Performance = R6Class("Performance",
  public = list(
    list.reward.epi = NULL,  # take reward vector of each episode
    list.discount.reward.epi = NULL,  # discounted reward per episode
    list.rewardPerEpisode = NULL,  # sum up reward of each episode
    list.discountedRPerEpisode = NULL,
    rewardPerStep = NULL,
    list.stepsPerEpisode = NULL,
    epi.idx = NULL,
    glogger = NULL,

    initialize = function(glogger) {
      self$glogger = glogger
      self$list.reward.epi = list()
      self$list.discount.reward.epi = list()
      self$epi.idx = 0L
      self$list.rewardPerEpisode = list()
      self$list.discountedRPerEpisode = list()
      self$list.stepsPerEpisode = list()
    },

    computeDiscount = function(rewardvec) {
      discounted_r = vector(mode = "double", length = length(rewardvec))
      running_add = 0
      i = length(rewardvec)
      while (i > 0) {
        running_add = running_add * 0.99 + rewardvec[i]
        discounted_r[i] = running_add
        i = i - 1L
      }
      discounted_r
    },

    persist = function(path) {
      perf = self$clone()
      save(perf, file = path)
    },


    getAccPerf = function(interval = 100L) {
      self$list.rewardPerEpisode = lapply(self$list.reward.epi, function(x) sum(x))
      epi.idx = length(self$list.rewardPerEpisode)
      winstart = max(1L, epi.idx - interval)
      vec = unlist(self$list.rewardPerEpisode)
      mean(vec[winstart:epi.idx])
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
      getAccPerf(100L)
    }
    ),
  private = list(),
  active = list())
