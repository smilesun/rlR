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
    episode.flag = NULL,
    vec.epi = NULL,
    initialize = function(rl.env, rl.agent) {
      self$continue.flag = TRUE
      self$perf = Performance$new()
      self$rl.agent = rl.agent
      self$rl.env = rl.env
      self$vec.epi = vector(mode = "numeric", length = 200L)  # gym episode stops at 200
      self$list.observers = list(
        "beforeAct" = list(
            hh = function() {
              log.nn$info("in episode %d, step %d", self$idx.episode, self$idx.step)
              self$rl.env$env$render()
              self$s.old = self$s_r_done_info[[1L]]}
        ),
        "afterStep" = list(
            function() {
              self$rl.agent$observe(self$s.old, self$action, self$s_r_done_info[[2L]], self$s_r_done_info[[1L]])
              self$vec.epi[self$idx.step] = s_r_done_info[[2L]]
              self$idx.step = self$idx.step + 1L
              self$rl.agent$replay(self$replayBatchSize)
            })
        )
    },

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
    }, finally = {
      self$rl.env$env$render(close = TRUE)
    }) # try catch
    } # function
    ), # public
  private = list(),
  active = list()
  )

