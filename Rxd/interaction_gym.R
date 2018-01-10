GymInteraction = R6Class("GymInteraction",
  inherit = Interaction,
  public = list(
    # replayBatchSize = RLConf$static$agent$replayBatchSize, never initialize
    # replayBatchSize = NULL,  # never initialize a variable in R6 directly like in java since this leads to contant non-changable configuration
    initialize = function(rl.env, rl.agent, maxiter, glogger) {
      self$perf = Performance$new()
      self$rl.agent = rl.agent
      self$rl.env = rl.env
      self$maxiter = maxiter 
      self$glogger = glogger
      # self$replayBatchSize = RLConf$static$agent$replayBatchSize  for R direct parameter initialization will stop later configuration since this might be executed earlier than the reconfiguration, so the reconfiguration could not change this class field anymore 
      },


    run = function() {
      s_r_done_info = NULL
      tryCatch({
      for(i in 1:self$maxiter) {
        self$glogger$log.nn$info("episodeStart %d ", i)
        s_r_done_info = self$rl.env$reset()  # start from a complete new random initial state
        self$perf$epi.idx = self$perf$epi.idx + 1L
        vec.epi = vector(mode = "numeric", length = 200L)  # gym episode stops at 200
        idx.step = 1L
          while(!s_r_done_info[[3L]]) {  # episode not finished
            self$glogger$log.nn$info("in episode %d, step %d", i, idx.step)
            self$rl.env$env$render()
            s.old = s_r_done_info[[1L]]
            assert(class(s.old) == "array")
            action = self$rl.agent$act(s.old)   # let agent decide which action to make, according to the current state
            s_r_done_info = self$rl.env$step(as.integer(action))
            self$rl.agent$observe(s.old, action, s_r_done_info[[2L]], s_r_done_info[[1L]])
            vec.epi[idx.step] = s_r_done_info[[2L]]
            idx.step = idx.step + 1L
            self$rl.agent$replay(RLConf$static$agent$replayBatchSize) # FIXME: stochastic replay (not each step) 
            } 
        self$perf$list.reward.epi[[self$perf$epi.idx]] = vec.epi   # the reward vector
        self$perf$list.stepsPerEpisode[[self$perf$epi.idx]] = idx.step -1L  # the number of steps
        # self$rl.agent$replay(idx.step -1L)  # update model after each episode is done, stupid ?
        cat(sprintf("Episode: %i, steps:%i \n", i, idx.step-1L))
      }  # for
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


