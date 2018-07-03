# The Discrete space allows a fixed range of non-negative numbers, so in this case valid actions are either 0 or 1
# The Box space represents an n-dimensional box, so valid observations will be an array of 4 numbers
# @note all processing to image should be consistent in EnvGym::reset, EnvGym::step and replaymem
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
    flag_stack_frame = NULL,
    old_dim = NULL,
    preprocess_dim = NULL,
    # act_cheat is a vector like c(5,7) which maps arm 1 to action 5 and arm 2 to action 7.
    # rendering the Pong-v0 in a ipy-notebook shows that the ball needs 20 frames to travel
    # observ_stack_len is the number of observations one should stack
    # observ_stack_len is set here since the Env should return the same dimension to the agent.
    # replaymem$sample.fun has to be changed if observ_stack is to be used.
    initialize = function(genv, name, state_preprocess = list(fun = identity, dim = NULL), act_cheat = NULL, ok_reward = NULL, ok_step = NULL, repeat_n_act = 1L, observ_stack_len = 1L) {
      self$flag_stack_frame = FALSE
      self$state_cache = vector(mode = "list", observ_stack_len)
      self$observ_stack_len = observ_stack_len
      self$env = genv
      self$flag_continous = ifelse(grepl("float", toString(genv$action_space$dtype)), TRUE, FALSE)
      self$name = name
      self$ok_reward = ok_reward
      self$ok_step = ok_step
      self$state_preprocess = state_preprocess$fun
      self$act_cheat = act_cheat
      self$repeat_n_act = repeat_n_act
      if (!"n" %in% names(genv$action_space)) {
        # if "n" is not in the names of the list, i.e. genv$action_space$n does not exist
        flag_multiple_shape = length(genv$action_space$shape) > 1L
        if (flag_multiple_shape) stop("currently no support for action space that have multiple shapes!")
        self$act_cnt = genv$action_space$shape[[1L]]
      } else {
        self$act_cnt = genv$action_space$n   # get the number of actions/control bits
      }
      if (!is.null(self$act_cheat)) self$act_cnt = length(self$act_cheat)
      self$state_dim = unlist(genv$observation_space$shape)
      if (is.null(self$state_dim)) stop("Compund state space Enviroment not supported!")
      self$flag_cnn = length(self$state_dim) > 1L  # judge if video input before change state_dim
      # FIXME: this should be set by user
      if (self$flag_cnn) self$flag_stack_frame = TRUE
      self$old_dim = self$state_dim
      self$preprocess_dim  = ifelse(is.null(state_preprocess$dim), self$old_dim, state_preprocess$dim)
      # keep the array order(only change the dimension) rather than increase the order
      # since two frames can be taken difference automatically by DNN and DNN only handles order-3D array
      #FIXME: by default, rlR handles up to order 3 tensor: RGB IMAGE. Might need to change in future
      if (self$flag_stack_frame) {
        self$state_preprocess = self$pong_cheat
        #self$preprocess_dim  = c(70L, 80L, 1L)
        self$preprocess_dim  = c(61L, 80L, 1L)
      }
      if (self$observ_stack_len > 1L) self$state_dim = c(self$preprocess_dim[1L:2L], self$observ_stack_len)
      else self$state_dim = self$preprocess_dim
    },

    pong_cheat = function(state) {
      I = state[seq(30L, 210L, 3L), seq(1L, 160L, 2L), ]
      I = 0.299 * I[, , 1L] + 0.587 * I[, , 2L] + 0.114 * I[, , 3L]
      res = array_reshape(I, c(dim(I), 1L))
      return(res)
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
      if (self$flag_stack_frame) s_r_d_info[["state"]] = self$stackLatestFrame(s_r_d_info[["state"]])
      #FIXME: might be buggy if continous space get preprocessed
      if (grepl("Box", toString(self$env$action_space))) s_r_d_info[["state"]] = t(s_r_d_info[["state"]])  # for continous action, transpose the state space, for "Pendulum-v0" etc, the state return is 3*1 instead of 1*3
      s_r_d_info
    },

    stackLatestFrame = function(cur_state) {
      if (self$observ_stack_len >= 2L) {
        for (i in self$observ_stack_len:2L) {
          self$state_cache[[i]] =  self$state_cache[[i - 1L]]
        }}
      self$state_cache[[1L]] = cur_state
      arr_stack = abind::abind(self$state_cache)
      #FIXME: How to initialize self$state_cache?
      return(arr_stack)
    },


    reset = function() {
      s = self$env$reset()
      s = self$state_preprocess(s)
      #FIXME: is this the right way to initialize old_state? reset is called at episode start?
      if (self$agent$interact$global_step_len == 0) {
        self$old_state = s
        self$state_cache = lapply(1L:self$observ_stack_len, function(x) s)
      }
      if (self$flag_stack_frame) {
        s = self$stackLatestFrame(s)
      }
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

    showImage = function(img) {
      img %>%
          imager::as.cimg() %>% # to image
          imager::mirror("y") %>% # mirror at y axis
          imager::imrotate(90L) %>% # rotate by 90 degree
          graphics::plot(axes = FALSE)
    },

    snapshot = function(steps = 25L) {
      checkmate::assert_int(steps)
      ss = self$env$reset()
      if (is.null(self$env$action_space$sample)) return("no support for snapshot for this environment")
      for (i in 1:steps) {
        a = self$env$action_space$sample()
        r = self$env$step(a)
      }
      img = self$env$render(mode = "rgb_array")
      img = img / 255.
      self$env$close()
      self$showImage(img)
    }
    )
)
