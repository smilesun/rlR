Performance = R6::R6Class("Performance",
  public = list(
    list.reward.epi = NULL,  # take reward vector of each episode
    list.discount.reward.epi = NULL,  # discounted reward per episode
    list.rewardPerEpisode = NULL,  # sum up reward of each episode
    list.discountedRPerEpisode = NULL,
    rewardPerStep = NULL,
    list.stepsPerEpisode = NULL,
    list.infos = NULL,
    epi.idx = NULL,
    glogger = NULL,
    agent = NULL,
    r.vec.epi = NULL,
    epi_wait_ini = NULL,   # number of episode to wait until to reinitialize
    epi_wait_expl = NULL,  # number of episode to wait until to increase epsilon for exploration
    recent_win = 20L,
    recent_door = 40L,
    bad_ratio = 0.99,
    gamma = NULL,

    initialize = function(agent) {
      self$agent = agent
      self$epi_wait_ini = self$agent$conf$get("policy.epi_wait_ini")
      self$epi_wait_expl = self$agent$conf$get("policy.epi_wait_expl")
      self$gamma = self$agent$conf$get("agent.gamma")
      self$glogger = self$agent$glogger
      self$list.reward.epi = list()
      self$list.infos = list()
      self$list.discount.reward.epi = list()
      self$epi.idx = 0L
      self$list.rewardPerEpisode = list()
      self$list.discountedRPerEpisode = list()
      self$list.stepsPerEpisode = list()
      self$r.vec.epi = vector(mode = "numeric", length = 2000L)  # FIXME: how to set a reasonble number here?
    },

    computeDiscount = function(rewardvec) {
      discounted_r = vector(mode = "double", length = length(rewardvec))
      running_add = 0
      i = length(rewardvec)
      while (i > 0) {
        running_add = running_add * self$gamma + rewardvec[i]
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
      mean(vec[winstart:epi.idx], na.rm = TRUE)
    },

    isBad = function() {
      pwin = self$getAccPerf(self$recent_win)
      pdoor = self$getAccPerf(self$recent_door)
      self$agent$interact$toConsole("Last %d episodes average reward %f \n", self$recent_win, pwin)
      self$agent$interact$toConsole("Last %d episodes average reward %f \n", self$recent_door, pdoor)
      flag1 = pwin < self$bad_ratio * pdoor
      flag2 = pwin < self$getAccPerf(100L)
      c(flag1, flag2)
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
      self$list.rewardPerEpisode = lapply(self$list.reward.epi, function(x) sum(x))
      rewards = unlist(self$list.rewardPerEpisode)
      df = data.frame(episode = seq_along(rewards),
        rewards = rewards)
      ggplot2::ggplot(df, aes(episode, rewards), col = "brown1") +
        geom_point(alpha = 0.2) +
        theme_bw() +
        labs(
          title = "Rewards Per Episode",
          x = "Episode",
          y = "Rewards per episode"
          ) +
        coord_cartesian(ylim = range(rewards)) +
        geom_smooth(se = FALSE, size = 1) +
        geom_hline(yintercept = median(rewards), size = 1, col = "black", lty = 2)
    },

    toScalar = function() {
      self$getAccPerf(100L)
    },

    extractInfo = function() {
      self$list.infos = lapply(self$agent$mem$samples, function(x) x$info)
    },

    rescue = function() {
      self$toString()   # print out performance
      if (self$glogger$flag) self$persist(self$agent$conf$conf.log.perf$resultTbPath)
      self$extractInfo()
    },

    afterEpisode = function() {
      self$epi.idx = self$epi.idx + 1L
      self$list.reward.epi[[self$epi.idx]] = vector(mode = "list")
      self$list.reward.epi[[self$epi.idx]] = self$r.vec.epi[1L:self$agent$interact$idx.step]   # the reward vector
      self$list.discount.reward.epi[[self$epi.idx]] = self$computeDiscount(self$r.vec.epi[1L:self$agent$interact$idx.step])
      self$list.stepsPerEpisode[[self$epi.idx]] = self$agent$interact$idx.step  # the number of steps
    }
    ),
  private = list(),
  active = list())
