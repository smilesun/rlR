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
    recent_win = NULL,
    epiLookBack = NULL,
    recent_door = NULL,
    bad_ratio = NULL,
    gamma = NULL,
    bad_reward = NULL,
    good_cnt = NULL,
    wait_epi = NULL,
    wait_cnt = NULL,
    reset_cnt = NULL,
    total.step = NULL,  # how many times model has been reset
    initialize = function(agent) {
      self$epiLookBack = 100L
      self$reset_cnt = 0L
      self$wait_epi = agent$conf$get("policy.epi_wait_ini")
      self$wait_cnt = 0L
      self$good_cnt = 0L
      self$recent_win = 20L
      self$recent_door = 40L
      self$bad_ratio = 0.99
      self$agent = agent
      if (!is.null(self$agent$env$bad_reward)) self$bad_reward = self$agent$env$bad_reward
      else self$bad_reward = -Inf

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

    success = function() {
      ok_reward = self$agent$env$ok_reward
      ok_step = self$agent$env$ok_step
      if (is.null(ok_reward) || is.null(ok_step)) {
        return(FALSE)
      }
      if (self$getAccPerf(ok_step) > ok_reward) {
        return(TRUE)
      }
      return(FALSE)
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
      all_rewards = unlist(self$list.rewardPerEpisode)
      flag1 = pwin < self$bad_ratio * pdoor
      flag2 = pwin < (1/self$bad_ratio) * self$getAccPerf(100L)
      flag2old = flag2
      flag3 = pwin < median(all_rewards)
      flag4 = pwin < mean(all_rewards)
      flag22 = (flag2 || flag2old)
      if (!flag22)  self$good_cnt = self$good_cnt + 1L
      else self$good_cnt = 0L
      res = c(flag1, flag2, flag3, flag4, flag22)
      names(res) = c("bad_small", "bad_middle", "bad_big1", "bad_big2", "bad_middle2")
      self$agent$interact$toConsole("%s", toString(res))
      return(res)
    },

    rescue = function() {
      flag = self$isBad()
      self$wait_epi = min(self$agent$conf$get("policy.epi_wait_expl"), self$wait_epi + 1)
      if (flag[1]) {
        self$agent$interact$toConsole("\n bad perform for last window, %d times \n", self$wait_cnt + 1L)
        self$wait_cnt = self$wait_cnt + 1L
        ratio = exp(-self$agent$policy$logdecay * self$total_step)
        #self$agent$policy$epsilon = min(1, self$agent$policy$epsilon * ratio)  #FIXME: shall we increase explore here ? Again and again exporation will never converge
        flag_new_start = self$wait_cnt > self$agent$conf$get("policy.epi_wait_middle")
        flag_start = all(flag) && flag_new_start
        if (self$wait_cnt > self$wait_epi || flag_start) {
          if (flag[2] || flag[3]) {
            self$agent$interact$toConsole("\n\n### going to reset brain ###\n\n\n")
            self$agent$setBrain()
            self$wait_epi = self$agent$conf$get("policy.epi_wait_expl")
            self$reset_cnt = self$reset_cnt + 1L
            self$agent$policy$epsilon = self$agent$policy$maxEpsilon
            self$wait_cnt = 0
          } else {
            self$wait_cnt = max(0, self$wait_cnt - 1)
            self$agent$policy$epsilon = self$agent$policy$maxEpsilon
          }
        }
      } else {
        if (self$good_cnt > 5L) {
          self$agent$interact$toConsole("\n# success more than 5 \n")
          self$wait_cnt = max(0, self$wait_cnt - self$agent$conf$get("policy.epi_wait_ini"))
      }}
      #else if (flag["bad_middle2"])
      # self$wait_cnt = max(0, self$wait_cnt - 1)
      # }
      self$agent$interact$toConsole("\n wait cnt: %d times \n", self$wait_cnt)
    }, # fun

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

    afterAll = function() {
      self$toString()   # print out performance
      ns = self$agent$conf$conf.log.perf$resultTbPath
      if (self$glogger$flag) self$persist(file.path(ns))
      self$extractInfo()
    },

    afterEpisode = function() {
      self$agent$interact$idx.episode = self$agent$interact$idx.episode + 1L
      self$agent$interact$glogger$log.nn$info("Episode: %i, steps:%i\n", self$agent$interact$idx.episode, self$agent$interact$idx.step)
      self$agent$interact$toConsole("Episode: %i finished with steps:%i \n", self$agent$interact$idx.episode, self$agent$interact$idx.step)
      self$epi.idx = self$epi.idx + 1L
      self$list.reward.epi[[self$epi.idx]] = vector(mode = "list")
      self$list.reward.epi[[self$epi.idx]] = self$r.vec.epi[1L:self$agent$interact$idx.step]   # the reward vector
      self$list.discount.reward.epi[[self$epi.idx]] = self$computeDiscount(self$r.vec.epi[1L:self$agent$interact$idx.step])
      self$list.stepsPerEpisode[[self$epi.idx]] = self$agent$interact$idx.step  # the number of steps
      rew = self$getAccPerf(self$epiLookBack)
      self$agent$interact$toConsole("Last %d episodes average reward %f \n", self$epiLookBack, rew)

    }
    ),
  private = list(),
  active = list())
