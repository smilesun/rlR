Interaction = R6::R6Class("Interaction",
  inherit = InteractionBase,
  private = list(
    list_observers = NULL,
    list_cmd = NULL,
    conf = NULL,
    continue_flag = NULL
  ),
  public = list(
    s_old = NULL,   # used in agent$observe
    action = NULL,
    s_r_done_info  = NULL,
    idx_episode = NULL,
    step_in_episode = NULL,
    maxiter = NULL,
    render = NULL,
    consoleFlag = NULL,
    printf = NULL,
    begin_learn = NULL,
    global_step_len = NULL,
    initialize = function(rl_env, rl_agent) {
      self$global_step_len = 0L
      self$rl_env = rl_env
      self$rl_agent = rl_agent
      private$conf = self$rl_agent$conf
      self$begin_learn = ifelse(is.null(private$conf$get("agent.start.learn")), private$conf$get("replay.batchsize"), private$conf$get("agent.start.learn"))
      checkmate::assert_int(self$begin_learn)
      self$render = private$conf$get("render")
      render_cmd = NULL
      if (self$render) render_cmd = "render"
      console_cmd = self$rl_agent$conf$get("console")
      if (is.null(console_cmd)) self$consoleFlag = FALSE
      else  self$consoleFlag = console_cmd
      if (self$consoleFlag) self$printf = function(str, ...) cat(sprintf(str, ...))
      else self$printf = function(str, ...) {
      }
      self$idx_episode = 0
      self$step_in_episode = 0
      private$continue_flag = TRUE
      self$glogger = self$rl_agent$glogger
      self$perf = Performance$new(self$rl_agent)
      private$list_cmd = list(
        "render" = self$rl_env$render,
        "before.act" = function() {
          self$glogger$log.nn$info("in episode %d, step %d, global step %d", self$idx_episode, self$step_in_episode, self$global_step_len)
          self$s_old = self$s_r_done_info[[1L]]
        },
        "after.step" = function() {
          self$glogger$log.nn$info("reward %f", self$s_r_done_info[[2L]])
          self$rl_agent$observe(self)
          self$perf$r.vec.epi[self$step_in_episode + 1L] = self$s_r_done_info[[2L]]
          self$step_in_episode = self$step_in_episode + 1L
          if (self$global_step_len > self$begin_learn) {
            self$rl_agent$afterStep()
          }
          self$checkEpisodeOver()
        })
      private$list_observers = list(
        "beforeAct" = private$list_cmd[c("before.act", render_cmd)],
        "afterStep" = private$list_cmd["after.step"]
        )
    },

    toConsole = function(str, ...) {
      do.call(self$printf, args = c(list(str = str), list(...)))
    },

    checkEpisodeOver = function() {
          if (self$s_r_done_info[[3L]]) {
            self$s_r_done_info = self$rl_env$reset()
            self$perf$afterEpisode()
            self$step_in_episode = 0L
            if (self$idx_episode >= self$maxiter) private$continue_flag = FALSE
            self$rl_agent$afterEpisode(self)
            # sucess_flag = self$perf$success()
            # if (sucess_flag) {
            #   private$continue_flag = FALSE
            # }
        }
        #gc()
    },

    notify = function(name) {
      flag = name %in% names(private$list_observers)
      if (!flag) stop("not defined observer")
      obslist = private$list_observers[[name]]
      for (method in names(obslist)) {
         do.call(obslist[[method]], args = list())
      }},

    run = function(maxiter) {
      self$step_in_episode = 0L
      self$idx_episode = 0L  # self$idx_episode is relative to maxiter, global episode is in perf 
      private$continue_flag = TRUE
      self$maxiter = maxiter
      self$s_r_done_info = self$rl_env$reset()
      tryCatch({
        while (private$continue_flag) {
          #self$notify("beforeAct")
          self$s_old = self$s_r_done_info[[1L]]
          self$action = self$rl_agent$act(self$s_old)
          #self$glogger$log.nn$info("action taken:%s \n", self$action)
          self$s_r_done_info = self$rl_env$step(self$action)
          self$global_step_len = self$global_step_len + 1L
          #self$notify("afterStep")
          self$rl_agent$observe()
          self$perf$r.vec.epi[self$step_in_episode + 1L] = self$s_r_done_info[[2L]]
          self$step_in_episode = self$step_in_episode + 1L
          if (self$global_step_len > self$begin_learn) {
            self$rl_agent$afterStep()
          }
          self$checkEpisodeOver()

        }
        self$perf$extractInfo()
        return(self$perf)
    }, finally = {
      self$rl_agent$sess$close()
      self$perf$afterAll()
      self$glogger$afterAll()
      self$rl_env$afterAll()
      rlR.global.perf <<- self$perf
      rlR.global.perf$agent$conf$updatePara("render", FALSE)
       }) # try catch
    }, # function run

    finanlize = function() {
      self$rl_agent$sess$close()
      self$perf$afterAll()
      self$glogger$afterAll()
      self$rl_env$afterAll()
      #rlR.global.perf <<- self$perf
      #rlR.global.perf$agent$conf$updatePara("render", FALSE)
    },


    run2 = function(maxiter) {
      self$step_in_episode = 0L
      self$idx_episode = 0L
      private$continue_flag = TRUE
      self$maxiter = maxiter
      self$s_r_done_info = self$rl_env$reset()
      tryCatch({
        while (private$continue_flag) {
          self$notify("beforeAct")
          self$action = self$rl_agent$act(self$s_old)
          self$glogger$log.nn$info("action taken:%s \n", self$action)
          self$s_r_done_info = self$rl_env$step(self$action)
          self$global_step_len = self$global_step_len + 1L
          self$notify("afterStep")
        }
        self$perf$extractInfo()
        return(self$perf)
    }, finally = {
      self$rl_agent$sess$close()
      self$perf$afterAll()
      self$glogger$afterAll()
      self$rl_env$afterAll()
      rlR.global.perf <<- self$perf
      rlR.global.perf$agent$conf$updatePara("render", FALSE)
       }) # try catch
    } # function run
  ) # public
)
