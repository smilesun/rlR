InteractionObserver = R6Class("InteractionObserver",
  inherit = Interaction,
  public = list(
    s_r_done_info  = NULL,
    list.observers = NULL,
    initialize = function(rl.env, rl.agent, maxiter) {
      self$perf = Performance$new(self)
      self$rl.agent = rl.agetn
      self$rl.env = rl.env
      self$maxiter = maxiter
    },

    virtual.makeObserverList = function() {
    },

    notify = function("") {
      list.observerst = lis[[""](
      for(name in names(self$list.observers)) {
         do.call(name)

      }
    #  self$rl.agent$observe(s.old, action, s_r_done_info[[2L]], s_r_done_info[[1L]])
    #  self$perf$list.reward.epi[[self$perf$epi.idx]] = vec.epi
    #  self$perf$list.stepsPerEpisode[[self$perf$epi.idx]] = idx.step -1L
    #  self$perf$idx.step = self$perf$idx.step + 1L
    #  self$rl.agent$replay(self$replayBatchSize)  # update model after each episode is done, stupid ?
    #},

    run = function() {
      self$s_r_done_info = self$rl.env$reset()
      tryCatch({
        while(self$continue.flag) {
          self$notify("beforeAct")
          action = self$rl.agent$act(s.old)
          self$notify("beforeStep")
          s_r_done_info = self$rl.env$step(as.integer(action))
          self$notify("afterStep")
        } # while 
    }, finally = {
      self$rl.env$env$render(close = TRUE)
    }) # try catch
    } # function
    ), # public
  private = list(),
  active = list()
  )

