
InteractionBase = R6Class("InteractionBase",
  public = list(
    rl.agent = NULL,
    rl.env = NULL,
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

