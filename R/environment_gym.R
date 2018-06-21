EnvGym = R6::R6Class("EnvGym",
  inherit = Environment,
  public = list(
    env = NULL,
    state.cheat = NULL,
    act.cheat = NULL,
    act_cnt = NULL,
    state_cnt = NULL,
    state_dim = NULL,
    ok_reward = NULL,
    ok_step = NULL,
    bad_reward = NULL,
    name = NULL,
    initialize = function(genv, name, state.cheat = identity, act.cheat = identity, actcnt = NULL, ok_reward = NULL, ok_step = NULL, bad_reward = NULL) {
      self$env = genv
      self$name = name
      self$ok_reward = ok_reward
      self$ok_step = ok_step
      self$bad_reward = bad_reward
      self$state.cheat = state.cheat
      self$act.cheat = act.cheat
      if (is.null(actcnt)) {
        self$act_cnt = genv$action_space$n   # get the number of actions/control bits
        if (is.null(genv$action_space$n)) stop("currently we do not support array action!")
        # currently no support for continuous action space
      } else {
        self$act_cnt = actcnt
      }
      self$state_dim = unlist(genv$observation_space$shape)
      state = genv$reset()  # only state is returned!
      state = self$state.cheat(state)
      self$state_cnt = length(state)
    },

    render = function(...) {
      self$env$render(...)
    },

    step = function(action) {
      action = action - 1L
      action = self$act.cheat(action)
      action = as.integer(action)
      s_r_d_info = self$env$step(action)
      names(s_r_d_info) = c("state", "reward", "done", "info")
      s_r_d_info[["state"]] = self$state.cheat(s_r_d_info[["state"]])
      sl = length(s_r_d_info[["state"]])
      s_r_d_info
    },

    reset = function() {
      s = self$env$reset()
      s = self$state.cheat(s)
      r = NULL
      return(list(s, r, FALSE, ""))
    },

    next.instance = function() {
      self$env$reset()
      r = NULL
      r
    },

    afterAll = function() {
      self$env$close()
    }
    ),
  private = list(),
  active = list()
  )
