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
    r.vec.epi = NULL,
    conf = NULL,
    beforeActPipe = NULL,
    afterStepPipe = NULL,
    list.cmd = NULL,
    replay.size = NULL,
    maxiter = NULL,
    render = NULL,
    initialize = function(rl.env, rl.agent, conf, glogger) {
      self$conf = conf
      self$render = self$conf$get("render")
      temp = NULL
      if (self$render) temp = "render"
      self$maxiter = conf$get("interact.maxiter")
      self$glogger = glogger
      self$continue.flag = TRUE
      self$episode.over.flag = FALSE
      self$perf = Performance$new(glogger)
      self$rl.agent = rl.agent
      self$rl.env = rl.env
      self$replay.size = self$conf$get("replay.batchsize")
      self$r.vec.epi = vector(mode = "numeric", length = 200L)  # gym episode stops at 200
      self$beforeActPipe = self$conf$get("interact.beforeActPipe")
      self$afterStepPipe = self$conf$get("interact.afterStepPipe")
      self$list.cmd = list(
        "render" = self$rl.env$env$render,
        "before.act" = function() {
          self$glogger$log.nn$info("in episode %d, step %d", self$idx.episode, self$idx.step)
          self$s.old = self$s_r_done_info[[1L]]
        },
        "after.step" = function() {
          self$glogger$log.nn$info("reward %f", self$s_r_done_info[[2L]])
          self$rl.agent$observe(state.old = self$s.old, action = self$action, reward = self$s_r_done_info[[2L]], state.new = self$s_r_done_info[[1L]], done = self$s_r_done_info[[3L]])
          self$r.vec.epi[self$idx.step] = self$s_r_done_info[[2L]]
          self$idx.step = self$idx.step + 1L
          self$checkEpisodeOver()
          self$rl.agent$afterStep()
        })
      self$list.observers = list(
        "beforeAct" = self$list.cmd[c("before.act", temp)],
        "afterStep" = self$list.cmd["after.step"]
        )
    },

    isEpisodeOver = function() {
      self$s_r_done_info[[3L]]
    },

    checkEpisodeOver = function() {
        if (self$s_r_done_info[[3L]]) {
          self$perf$epi.idx = self$perf$epi.idx + 1L
          self$episode.over.flag = TRUE
          self$idx.episode = self$idx.episode + 1L
          self$rl.agent$epi.idx = self$idx.episode
          self$rl.env$reset()
          self$perf$list.reward.epi[[self$perf$epi.idx]] = vector(mode = "list")
          self$perf$list.reward.epi[[self$perf$epi.idx]] = self$r.vec.epi[1L:self$idx.step]   # the reward vector
          self$perf$list.discount.reward.epi[[self$perf$epi.idx]] = self$perf$computeDiscount(self$r.vec.epi[1L:self$idx.step])
          self$glogger$log.nn$info("Episode: %i, steps:%i\n", self$idx.episode, self$idx.step)
          cat(sprintf("Episode: %i finished with steps:%i \n", self$idx.episode, self$idx.step))  # same message to console
          self$perf$list.stepsPerEpisode[[self$perf$epi.idx]] = self$idx.step  # the number of steps

          rew = self$perf$getAccPerf(100)
          cat(sprintf("Last %d episodes average reward %f \n", 100, rew))  # same message to console
          self$idx.step = 0L
          self$episode.over.flag = FALSE
          if (self$idx.episode > self$maxiter) {
            self$continue.flag = FALSE
          }
          self$rl.agent$afterEpisode(self)
    }},

    notify = function(name) {
      if (name %nin% names(self$list.observers)) stop("not defined observer")
      obslist = self$list.observers[[name]]
      for (method in names(obslist)) {
         do.call(obslist[[method]], args = list())
      }},

    run = function(maxiter = NULL) {
      if (!is.null(maxiter)) self$maxiter = maxiter
      self$s_r_done_info = self$rl.env$reset()
      tryCatch({
        while (self$continue.flag) {
          self$notify("beforeAct")
          self$action = self$rl.agent$act(self$s.old)   # policy decides the convention
          self$glogger$log.nn$info("action taken:%i \n", self$action)
          self$s_r_done_info = self$rl.env$step(action = as.integer(self$action))
          self$notify("afterStep")
        }
        return(self$perf)
    }, finally = {
      self$perf$toString()   # print out performance
      self$perf$persist(self$conf$conf.log.perf$resultTbPath)
      filename.replay = file.path(rlR.conf4log$filePrefix, "replay.dt.csv")
      filename.experience = file.path(self$conf$conf.log.perf$filePrefix, "experience.dt.csv")
      self$glogger$log.root$info("\n a = BBmisc::load2('%s')", self$conf$conf.log.perf$resultTbPath)
      cat(sprintf("\n a = BBmisc::load2('%s')", self$conf$conf.log.perf$resultTbPath))
      write.csv(self$rl.agent$mem$dt, file = filename.experience)
      self$glogger$log.root$info("\n b = read.csv('%s')", filename.experience)
      self$rl.env$env$render(close = TRUE)
      perf <<- self$perf
    }) # try catch
    } # function
    ), # public
  private = list(),
  active = list()
  )
