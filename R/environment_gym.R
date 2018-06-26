# The Discrete space allows a fixed range of non-negative numbers, so in this case valid actions are either 0 or 1
# The Box space represents an n-dimensional box, so valid observations will be an array of 4 numbers
EnvGym = R6::R6Class("EnvGym",
  inherit = Environment,
  public = list(
    env = NULL,
    ok_reward = NULL,
    ok_step = NULL,
    state_cheat = NULL,
    act_cheat = NULL,
    old_state = NULL,  # for video
    video_history_len = NULL, # number of identical actions to excert
    flag_video = NULL,
    #act_cheat should be
    initialize = function(genv, name, state_cheat = identity, act_cheat = NULL, ok_reward = NULL, ok_step = NULL) {
      self$env = genv
      self$flag_continous = ifelse(grepl("float", toString(genv$action_space$dtype)), TRUE, FALSE)
      self$name = name
      self$ok_reward = ok_reward
      self$ok_step = ok_step
      self$state_cheat = state_cheat
      self$act_cheat = act_cheat
      if (!"n" %in% names(genv$action_space)) {
        # if "n" is not in the names of the list, i.e. genv$action_space$n does not exist
        flag_multiple_shape = length(genv$action_space$shape) > 1L
        if (flag_multiple_shape) stop("currently we do not action space that have multiple shapes!")
        self$act_cnt = genv$action_space$shape[[1]]
      } else {
        self$act_cnt = genv$action_space$n   # get the number of actions/control bits
      }
      if (!is.null(self$act_cheat)) self$act_cnt = length(self$act_cheat)
      self$state_dim = unlist(genv$observation_space$shape)
      state = genv$reset()  # in gym, only state is returned!
      state = self$state_cheat(state)
      self$old_state = state  # for video
      self$flag_video = length(self$state_dim) > 1L
      self$video_history_len = 4
    },

    render = function(...) {
      self$env$render(...)
    },

    step = function(action) {
      if (!is.null(self$act_cheat)) action = self$act_cheat[action]
      if (!self$flag_continous) {
        action = action - 1L
        action = as.integer(action)
      }
      s_r_d_info = self$env$step(action)
      names(s_r_d_info) = c("state", "reward", "done", "info")
      # for "CartPole-v0" etc, the state return is vector instead of state
      cur = self$state_cheat(s_r_d_info[["state"]])
      if (grepl("Box", toString(self$env$action_space))) cur = t(cur)  # for continous action, transpose the state space, for "Pendulum-v0" etc, the state return is 3*1 instead of 1*3
      if (self$flag_video) {
        s_r_d_info = self$env$step(action)
        s_r_d_info = self$env$step(action)
        s_r_d_info = self$env$step(action)
        names(s_r_d_info) = c("state", "reward", "done", "info")
        cur = self$state_cheat(s_r_d_info[["state"]])
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
      s = self$state_cheat(s)
      r = NULL
      return(list(s, r, FALSE, ""))
    },

    afterAll = function() {
      self$env$close()
    }
    ),
  private = list(),
  active = list()
  )
