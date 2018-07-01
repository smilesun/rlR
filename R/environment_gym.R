# The Discrete space allows a fixed range of non-negative numbers, so in this case valid actions are either 0 or 1
# The Box space represents an n-dimensional box, so valid observations will be an array of 4 numbers
EnvGym = R6::R6Class("EnvGym",
  inherit = Environment,
  public = list(
    env = NULL,
    ok_reward = NULL,
    ok_step = NULL,
    state_preprocess = NULL,
    act_cheat = NULL,
    old_state = NULL,  # for video
    repeat_n_act = NULL,  # number of frames to escape
    state_cache = NULL,   # store adjacent states to stack into short history
    flag_diff_video = NULL,
    # act_cheat is a vector like c(5,7) which maps arm 1 to action 5 and arm 2 to action 7.
    # rendering the Pong-v0 in a ipy-notebook shows that the ball needs 20 frames to travel
    # observ_stack_len is the number of observations one should stack
    # observ_stack_len is set here since the Env should return the same dimension to the agent.
    # replaymem$sample.fun has to be changed if observ_stack is to be used.
    initialize = function(genv, name, state_preprocess = identity, act_cheat = NULL, ok_reward = NULL, ok_step = NULL, repeat_n_act = 1L, observ_stack_len =1L, flag_diff_video = FALSE) {
      self$flag_diff_video = flag_diff_video
      self$state_cache = vector(mode = "list", observ_stack_len)
      self$observ_stack_len = observ_stack_len
      self$env = genv
      self$flag_continous = ifelse(grepl("float", toString(genv$action_space$dtype)), TRUE, FALSE)
      self$name = name
      self$ok_reward = ok_reward
      self$ok_step = ok_step
      self$state_preprocess = state_preprocess
      self$act_cheat = act_cheat
      self$repeat_n_act = repeat_n_act
      if (!"n" %in% names(genv$action_space)) {
        # if "n" is not in the names of the list, i.e. genv$action_space$n does not exist
        flag_multiple_shape = length(genv$action_space$shape) > 1L
        if (flag_multiple_shape) stop("currently no support for action space that have multiple shapes!")
        self$act_cnt = genv$action_space$shape[[1]]
      } else {
        self$act_cnt = genv$action_space$n   # get the number of actions/control bits
      }
      if (!is.null(self$act_cheat)) self$act_cnt = length(self$act_cheat)
      self$state_dim = unlist(genv$observation_space$shape)
      state = genv$reset()  # in gym, only state is returned!
      if (self$flag_diff_video) self$state_preprocess = self$pong_cheat
      state = self$state_preprocess(state)
      self$old_state = state  # for video
      self$flag_cnn = length(self$state_dim) > 1L
      old_dim = self$state_dim
      # increase the order rather than keep the array order(only change the dimension)
      # since two frames should not be considered the same
      ##if (self$observ_stack_len > 1L) self$state_dim = c(self$observ_stack_len, old_dim)
    },

    pong_cheat = function(state) {
      I = state[seq(1, 210, 3), seq(1,160,2), 1]
      self$state_dim = c(dim(I),1)
      if (self$observ_stack_len > 1L) self$state_dim = c(old_dim, self$observ_stack_len)
      res = array_reshape(I, c(dim(I),1))
    },

    render = function(...) {
      self$env$render(...)
    },

    step = function(action_input) {
      action = action_input
      if (!is.null(self$act_cheat)) {
        # act_cheat must be applied before minus 1 operation below since R has no 0 index!
        action = as.integer(self$act_cheat[action] + 1L)  # gym convention here
      }
      if (!self$flag_continous) {
        action = action - 1L  # The class in which the current code lies is Gym Specific
        action = as.integer(action)
      }
      list_s_r_d_info = lapply(1:self$repeat_n_act, function(i) self$env$step(action))
      rewards = sapply(list_s_r_d_info, function(x) x[[2L]])
      s_r_d_info = list_s_r_d_info[[self$repeat_n_act]]
      names(s_r_d_info) = c("state", "reward", "done", "info")
      s_r_d_info[["reward"]] = sum(rewards)
      s_r_d_info[["state"]] = self$state_preprocess(s_r_d_info[["state"]])  # preprocessing
      if (self$flag_diff_video) s_r_d_info[["state"]] = self$takeDiff(s_r_d_info)
      #FIXME: might be buggy if continous space get preprocessed
      if (grepl("Box", toString(self$env$action_space))) s_r_d_info[["state"]] = t(s_r_d_info[["state"]])  # for continous action, transpose the state space, for "Pendulum-v0" etc, the state return is 3*1 instead of 1*3
      s_r_d_info
    },

    takeDiff = function(s_r_d_info) {
      if (self$flag_diff_video) {
        cur = s_r_d_info[["state"]]
        arr_stack = sapply(list(cur, self$old_state), identity, simplify = "array")
        browser()
        s_r_d_info[["state"]] = cur - self$old_state
        self$old_state = cur   # How to initialize self$old_state?
      }
    },

    reset = function() {
      s = self$env$reset()
      s = self$state_preprocess(s)
      r = NULL
      return(list(s, r, FALSE, ""))
    },

    afterAll = function() {
      self$env$close()
    },

    randomRun = function(steps, render = TRUE) {
      checkmate::assert_int(steps)
      ss = self$env$reset()
      for (i in 1:steps) {
        if (render) env$render()
        a = env$action_space$sample()
        r = env$step(a)
      }
      env$close()
      env
    },

    snapshot = function(steps = 25L) {
      checkmate::assert_int(steps)
      ss = self$env$reset()
      for (i in 1:steps) {
        a = env$action_space$sample()
        r = env$step(a)
      }
      img = env$render(mode = 'rgb_array')
      img = img / 255.
      env$close()
      img %>%
      imager::as.cimg() %>% # to image
      imager::mirror("y") %>% # mirror at y axis
      imager::imrotate(90L) %>% # rotate by 90 degree
      graphics::plot(axes = FALSE)
    }
    )
)
