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
    initialize = function(rl.env, rl.agent) {
      self$continue.flag = TRUE
      self$episode.over.flag = FALSE
      self$perf = Performance$new()
      self$rl.agent = rl.agent
      self$rl.env = rl.env
      self$vec.epi = vector(mode = "numeric", length = 200L)  # gym episode stops at 200
      self$list.observers = list(
        "beforeAct" = list(
            hh = function() {
              self$rl.env$env$render() 
              glogger$log.nn$info("in episode %d, step %d", self$idx.episode, self$idx.step)
              self$s.old = self$s_r_done_info[[1L]]}
        ),

        "afterStep" = list(
            hh = function() {
              glogger$log.nn$info("reward %f", self$s_r_done_info[[2L]])
              self$rl.agent$observe(self$s.old, self$action, self$s_r_done_info[[2L]], self$s_r_done_info[[1L]])
              self$vec.epi[self$idx.step] = self$s_r_done_info[[2L]]
              self$idx.step = self$idx.step + 1L
              self$rl.agent$replay(RLConf$static$agent$replayBatchSize)
              self$checkEpisodeOver()
            })
        )
    },

    checkEpisodeOver = function() {
        if(self$s_r_done_info[[3L]]) {
          self$perf$epi.idx = self$perf$epi.idx + 1L
          self$episode.over.flag = TRUE
          self$idx.episode = self$idx.episode + 1L
          self$rl.env$reset()
          self$perf$list.reward.epi[[self$perf$epi.idx]] = vector(mode = "list")
          self$perf$list.reward.epi[[self$perf$epi.idx]] = self$vec.epi[1L:self$idx.step]   # the reward vector
          glogger$log.root$info("Episode: %i, steps:%i \n", self$idx.episode, self$idx.step)
          self$idx.step = 0L
          self$episode.over.flag = FALSE
          self$perf$list.stepsPerEpisode[[self$perf$epi.idx]] = self$idx.step -1L  # the number of steps
          if(self$idx.episode > RLConf$static$interact$maxiter) {
            self$continue.flag = FALSE }
    }},

    notify = function(name) {
      if(name %nin% names(self$list.observers)) stop("not defined observer")
      obslist = self$list.observers[[name]]
      for(method in names(obslist)) {
         do.call(obslist[[method]], args = list())
      }},

    run = function() {
      # self$notify("beforeRun")
      self$s_r_done_info = self$rl.env$reset()
      tryCatch({
        while(self$continue.flag) {
          self$notify("beforeAct")
          self$action = self$rl.agent$act(self$s.old)
          self$s_r_done_info = self$rl.env$step(as.integer(self$action))
          self$notify("afterStep")
        } # while 
        return(self$perf)
    }, finally = {
      self$perf$toString()
      self$rl.env$env$render(close = TRUE)
    }) # try catch
    } # function
    ), # public
  private = list(),
  active = list()
  )

