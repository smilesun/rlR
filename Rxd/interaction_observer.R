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

    notify = function(name) {
      obslist = list[[name]]
      for(name in names(obslist)) {
         do.call(obs[[name]])
      }},

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

