
Interaction = R6Class("Interaction",
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

