InteractionBase = R6::R6Class("InteractionBase",
  public = list(
    rl_agent = NULL,
    rl_env = NULL,
    perf = NULL,
    maxiter = NULL,
    glogger = NULL,
    run = function() {
      stop("not implemented")
    }
    ), # public
  private = list(),
  active = list()
  )
