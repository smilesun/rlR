# The Discrete space allows a fixed range of non-negative numbers, so in this case valid actions are either 0 or 1
# The Box space represents an n-dimensional box, so valid observations will be an array of 4 numbers
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
    old_state = NULL,
    flag_video = NULL,
    flag_continous = NULL,
    initialize = function(genv, name, state.cheat = identity, act.cheat = identity, actcnt = NULL, ok_reward = NULL, ok_step = NULL, bad_reward = NULL) {
      self$env = genv
      self$flag_continous = ifelse(grepl("float", toString(genv$action_space$dtype)), TRUE, FALSE)
      self$name = name
      self$ok_reward = ok_reward
      self$ok_step = ok_step
      self$bad_reward = bad_reward
      self$state.cheat = state.cheat
      self$act.cheat = act.cheat
      if (is.null(actcnt)) {
        if (!"n" %in% names(genv$action_space)) {
          flag_multiple_shape = length(genv$action_space$shape) > 1L
          if (flag_multiple_shape) stop("currently we do not action space that have multiple shapes!")
          self$act_cnt = genv$action_space$shape[[1]]
        } else {
          self$act_cnt = genv$action_space$n   # get the number of actions/control bits
        }
      } else {
        self$act_cnt = actcnt
      }
      self$state_dim = unlist(genv$observation_space$shape)
      state = genv$reset()  # only state is returned!
      state = self$state.cheat(state)
      self$old_state = state
      self$flag_video = length(self$state_dim) > 1L
      self$state_cnt = length(state)
    },

    render = function(...) {
      self$env$render(...)
    },

    step = function(action) {
      if (!self$flag_continous) {
        action = action - 1L
        action = as.integer(action)
      }
      action = self$act.cheat(action)
      s_r_d_info = self$env$step(action)
      names(s_r_d_info) = c("state", "reward", "done", "info")
      # for "Pendulum-v0" etc, the state return is 3*1 instead of 1*3
      # for "CartPole-v0" etc, the state return is vector instead of state
      cur = self$state.cheat(s_r_d_info[["state"]])
      if (grepl("Box", toString(self$env$action_space))) cur = t(cur)  # for continous action
      if (self$flag_video) {
        s_r_d_info[["state"]] = cur - self$old_state
        self$old_state = cur
      } else {
        s_r_d_info[["state"]] = cur
      }
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
