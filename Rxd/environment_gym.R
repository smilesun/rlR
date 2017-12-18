EnvGym = R6Class("EnvGym",
  inherit = Environment,
  public = list(
    env = NULL,
    initialize = function(env) {
      self$env = env
    },

    map = function(name) {
      list("MountainCar":c(0, 2)) # omit the 1 action which is doing nothing, mapping 1 to 2
    },

    step = function(action) {
      action = as.integer(action)
      s_r_d_info = self$env$step(action)
      names(s_r_d_info) = c("state", "reward", "done", "info")
      s_r_d_info
    },

    reset = function() {
      s = self$env$reset()
      r = NULL
      return(list(s, r, FALSE, ""))
    },

    next.instance = function() {
      self$env$reset()
      r = NULL
      r
    }
    ),
  private = list(),
  active = list()
  )


