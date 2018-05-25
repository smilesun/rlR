Interaction = R6::R6Class("Interaction",
  inherit = InteractionBase,
  public = list(
    epiLookBack = 100L,
    s.old = NULL,
    action = NULL,
    s_r_done_info  = NULL,
    list.observers = NULL,
    idx.episode = NULL,
    idx.step = NULL,
    continue.flag = NULL,
    conf = NULL,
    list.cmd = NULL,
    maxiter = NULL,
    render = NULL,
    consoleFlag = NULL,
    printf = NULL,
    initialize = function(rl.env, rl.agent) {
      self$rl.env = rl.env
      self$rl.agent = rl.agent
      self$conf = self$rl.agent$conf
      self$render = self$conf$get("render")
      render_cmd = NULL
      if (self$render) render_cmd = "render"
      console_cmd = self$rl.agent$conf$get("console")
      if (is.null(console_cmd)) self$consoleFlag = FALSE
      else  self$consoleFlag = console_cmd
      if (self$consoleFlag) self$printf = function(str, ...) cat(sprintf(str, ...))
      else self$printf = function(str, ...) {
      }
      self$maxiter = self$conf$get("interact.maxiter")
      self$idx.episode = 0
      self$idx.step = 0
      self$continue.flag = TRUE
      self$glogger = self$rl.agent$glogger
      self$perf = Performance$new(self$rl.agent)
      self$list.cmd = list(
        "render" = self$rl.env$render,
        "before.act" = function() {
          self$glogger$log.nn$info("in episode %d, step %d", self$idx.episode, self$idx.step)
          self$s.old = self$s_r_done_info[[1L]]
        },
        "after.step" = function() {
          self$glogger$log.nn$info("reward %f", self$s_r_done_info[[2L]])
          self$rl.agent$observe(state.old = self$s.old, action = self$action, reward = self$s_r_done_info[[2L]], state.new = self$s_r_done_info[[1L]], done = self$s_r_done_info[[3L]], info = self$s_r_done_info[[4]], episode = self$idx.episode + 1L, stepidx = self$idx.step + 1L)
          self$perf$r.vec.epi[self$idx.step] = self$s_r_done_info[[2L]]
          self$idx.step = self$idx.step + 1L
          self$rl.agent$afterStep()
          self$checkEpisodeOver()
        })
      self$list.observers = list(
        "beforeAct" = self$list.cmd[c("before.act", render_cmd)],
        "afterStep" = self$list.cmd["after.step"]
        )
    },

    isEpisodeOver = function() {
      self$s_r_done_info[[3L]]
    },

    toConsole = function(str, ...) {
      do.call(self$printf, args = c(list(str = str), list(...)))
    },

    checkEpisodeOver = function() {
        if (self$s_r_done_info[[3L]]) {
          self$rl.env$reset()
          self$idx.episode = self$idx.episode + 1L
          self$glogger$log.nn$info("Episode: %i, steps:%i\n", self$idx.episode, self$idx.step)
          self$toConsole("Episode: %i finished with steps:%i \n", self$idx.episode, self$idx.step)
          self$perf$afterEpisode()
          rew = self$perf$getAccPerf(self$epiLookBack)
          self$toConsole("Last %d episodes average reward %f \n", self$epiLookBack, rew)
          self$idx.step = 0L
          if (self$idx.episode >= self$maxiter) {
            self$continue.flag = FALSE
          }
          self$rl.agent$afterEpisode(self)
          sucess_flag = self$perf$success()
          if (sucess_flag) {
            self$continue.flag = FALSE
          }
        }
    },

    notify = function(name) {
      flag = name %in% names(self$list.observers)
      if (!flag) stop("not defined observer")
      obslist = self$list.observers[[name]]
      for (method in names(obslist)) {
         do.call(obslist[[method]], args = list())
      }},

    run = function(maxiter = NULL) {
      self$idx.step = 0L
      self$idx.episode = 0L
      self$continue.flag = TRUE
      if (!is.null(maxiter)) self$maxiter = maxiter
      self$s_r_done_info = self$rl.env$reset()
      tryCatch({
        while (self$continue.flag) {
          self$notify("beforeAct")
          self$action = self$rl.agent$act(self$s.old)
          self$glogger$log.nn$info("action taken:%i \n", self$action)
          self$s_r_done_info = self$rl.env$step(action = as.integer(self$action))
          self$notify("afterStep")
        }
        self$perf$extractInfo()
        return(self$perf)
    }, finally = {
      self$perf$afterAll()
      self$glogger$afterAll()
      self$rl.env$afterAll()
      rlR.global.perf <<- self$perf
      rlR.global.perf$agent$conf$updatePara("render", FALSE)
      rlR.rescue.global.agent <<- self$rl.agent$clone(deep = TRUE)
      rlR.rescue.global.agent$updatePara("render", FALSE)
      rlR.rescue.global.inter <<- self
    }) # try catch
    } # function
    ), # public
  private = list(),
  active = list()
  )

continue = function() rlR.rescue.global.agent$learn(1000)
