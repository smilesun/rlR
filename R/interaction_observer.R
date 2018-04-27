InteractionObserver = R6Class("InteractionObserver",
  inherit = Interaction,
  public = list(
    s.old = NULL,
    action = NULL,
    s_r_done_info  = NULL,
    list.observers = NULL,
    idx.episode = 0,
    idx.step = 0,
    continue.flag = NULL,
    episode.over.flag = NULL,
    vec.epi = NULL,
    conf = NULL,
    beforeActPipe = NULL,
    afterStepPipe = NULL,
    list.cmd = NULL,
    replay.size = NULL,
    initialize = function(rl.env, rl.agent, conf, glogger) {
      self$conf = conf
      self$glogger = glogger
      self$continue.flag = TRUE
      self$episode.over.flag = FALSE
      self$perf = Performance$new(glogger)
      self$rl.agent = rl.agent
      self$rl.env = rl.env
      self$replay.size = self$conf$get("replay.batchsize")
      self$vec.epi = vector(mode = "numeric", length = 200L)  # gym episode stops at 200
      self$beforeActPipe = self$conf$get("interact.beforeActPipe")
      self$afterStepPipe = self$conf$get("interact.afterStepPipe")
      self$list.cmd = list(
        "render" = self$rl.env$env$render,
        "epi-step-log" = function() {
          self$glogger$log.nn$info("in episode %d, step %d", self$idx.episode, self$idx.step)
          self$s.old = self$s_r_done_info[[1L]]
        },
        "after.step" = function() {
          self$glogger$log.nn$info("reward %f", self$s_r_done_info[[2L]])
          self$rl.agent$observe(self$s.old, self$action, self$s_r_done_info[[2L]], self$s_r_done_info[[1L]])
          self$perf$list.rewardPerEpisode
          self$vec.epi[self$idx.step] = self$s_r_done_info[[2L]]
          self$idx.step = self$idx.step + 1L
          self$checkEpisodeOver()
        },

        "replay.perEpisode.all" = function() {
          n = length(self$rl.agent$mem$samples) 
          if (self$s_r_done_info[[3L]]) {
            total.reward = sum(self$perf$list.reward.epi[[self$perf$epi.idx]])
            total.step = unlist(self$perf$list.stepsPerEpisode)[self$perf$epi.idx]
            adg = total.reward / total.step
            self$rl.agent$setAdvantage(adg)
            self$rl.agent$replay(n)   # key difference here
          }

        },

        "replay" = function() {
          self$rl.agent$replay(self$replay.size)
        },
        "replayPerEpisode" = function() {
          if (self$s_r_done_info[[3L]]) {
            total.reward = sum(self$perf$list.reward.epi[[self$perf$epi.idx]])
            total.step = unlist(self$perf$list.stepsPerEpisode)[self$perf$epi.idx]
            adg = total.reward / total.step
            self$rl.agent$setAdvantage(adg)
            self$rl.agent$replay(total.step)   # key difference here
          }
        })
      self$list.observers = list(
        "beforeAct" = self$list.cmd[self$beforeActPipe],
        "afterStep" = self$list.cmd[self$afterStepPipe]
        )
    },

    checkEpisodeOver = function() {
        if (self$s_r_done_info[[3L]]) {
          self$perf$epi.idx = self$perf$epi.idx + 1L
          self$episode.over.flag = TRUE
          self$idx.episode = self$idx.episode + 1L
          self$rl.agent$epi.idx = self$idx.episode
          self$rl.env$reset()
          self$perf$list.reward.epi[[self$perf$epi.idx]] = vector(mode = "list")
          self$perf$list.reward.epi[[self$perf$epi.idx]] = self$vec.epi[1L:self$idx.step]   # the reward vector
          self$glogger$log.root$info("Episode: %i, steps:%i \n", self$idx.episode, self$idx.step)
          cat(sprintf("Episode: %i, steps:%i \n", self$idx.episode, self$idx.step))  # same message to console
          self$perf$list.stepsPerEpisode[[self$perf$epi.idx]] = self$idx.step - 1L  # the number of steps
          self$idx.step = 0L
          self$episode.over.flag = FALSE
          if (self$idx.episode > self$conf$get("interact.maxiter")) {
            self$continue.flag = FALSE
          }
          temp = self$rl.agent$epsilon * self$conf$get("policy.decay")
          self$rl.agent$epsilon = temp
          cat(sprintf("Epsilon%f \n", temp))  # same message to console
    }},

    notify = function(name) {
      if (name %nin% names(self$list.observers)) stop("not defined observer")
      obslist = self$list.observers[[name]]
      for (method in names(obslist)) {
         do.call(obslist[[method]], args = list())
      }},

    run = function() {
      self$s_r_done_info = self$rl.env$reset()
      tryCatch({
        while (self$continue.flag) {
          self$notify("beforeAct")
          self$action = self$rl.agent$act(self$s.old)   # policy decides the convention
          self$s_r_done_info = self$rl.env$step(action = as.integer(self$action))
          self$notify("afterStep")
        }
        return(self$perf)
    }, finally = {
      self$perf$toString()   # print out performance
      self$rl.env$env$render(close = TRUE)
    }) # try catch
    } # function
    ), # public
  private = list(),
  active = list()
  )

