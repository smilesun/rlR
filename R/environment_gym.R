# The Discrete space allows a fixed range of non-negative numbers, so in this case valid actions are either 0 or 1
# The Box space represents an n-dimensional box, so valid observations will be an array of 4 numbers
# @note all processing to image should be consistent in EnvGym::reset, EnvGym::step and replaymem
#     subsample = function(state) {
#      I = state[seq(30L, 210L, 3L), seq(1L, 160L, 2L), ]
#      I = 0.299 * I[, , 1L] + 0.587 * I[, , 2L] + 0.114 * I[, , 3L]  # RGB to gray formula
#      res = array_reshape(I, c(dim(I), 1L))
#      res = array(as.integer(res), dim = dim(res))  # store integer is less memory hungry
#      return(res)
#    }


EnvGym = R6::R6Class("EnvGym",
  inherit = Environment,
  private = list(
    # more elements in private is more stable, but makes debug harder
    old_dim = NULL,  # orginal dimension
    new_dim = NULL,
    old_state = NULL,  # for video to remove flickering
    initActCnt = function() {
      if (!"n" %in% names(self$env$action_space)) {
        # if "n" is not in the names of the list, i.e. genv$action_space$n does not exist
        flag_multiple_shape = length(self$env$action_space$shape) > 1L
        if (flag_multiple_shape) {
          stop("currently no support for action space that have multiple shapes!")
        }
        self$act_cnt = self$env$action_space$shape[[1L]]
      } else {
        self$act_cnt = self$env$action_space$n   # get the number of actions/control bits
      }

      if (!is.null(self$act_cheat)) {
        self$act_cnt = length(self$act_cheat)
      }

    },

    initStateDim = function() {
      self$state_dim = unlist(self$env$observation_space$shape)
      if (is.null(self$state_dim)) {
        stop("Compund state space Enviroment not supported!")
      }
      self$flag_tensor = length(self$state_dim) > 1L  # judge if video input before change state_dim
      # FIXME: this should be set by user
      if (self$flag_tensor) {
        # since which("NOOP" == env$env$unwrapped$get_action_meanings()) will always generate 1
        self$act_cheat = 1L:(self$act_cnt - 1L)
        self$act_cnt = self$act_cnt - 1L
        self$state_preprocess = self$subsample
      }
      private$old_dim = self$state_dim
      # keep the array order(only change the dimension) rather than increase the order
      # since two frames can be taken difference automatically by DNN and DNN only handles order-3D array
      #FIXME: by default, rlR handles up to order 3 tensor: RGB IMAGE. Might need to change in future
      private$new_dim  = self$initSubsample()
      if (self$observ_stack_len > 1L) {
        self$flag_stack_frame = TRUE
        self$state_dim = c(private$new_dim[1L:2L], self$observ_stack_len)
      } else {
        self$state_dim = private$new_dim  # no stacking
      }
    }
 ),

  public = list(
    # some fields are defined in father class
    env = NULL,
    ok_reward = NULL,
    ok_step = NULL,
    state_preprocess = NULL,
    act_cheat = NULL,
    repeat_n_act = NULL,  # number of frames to escape
    state_cache = NULL,   # store adjacent states to stack into short history
    flag_stack_frame = NULL,
    flag_tensor = NULL,
    # act_cheat is a vector like c(5,7) which maps arm 1 to action 5 and arm 2 to action 7.
    # rendering the Pong-v0 in a ipy-notebook shows that the ball needs 20 frames to travel
    # observ_stack_len is the number of observations one should stack, but will not change the order of the state tensor
    # observ_stack_len is set here since the Env should return the same dimension to the agent.
    # replaymem$sample.fun has to be changed if observ_stack is to be used.
    # subsample_dim: the size of image after subsampling
    initialize = function(genv, name, state_preprocess = list(fun = identity, par = NULL), act_cheat = NULL, ok_reward = NULL, ok_step = NULL, repeat_n_act = 1L, observ_stack_len = 1L) {
      self$flag_stack_frame = FALSE
      self$state_cache = vector(mode = "list", observ_stack_len)
      self$observ_stack_len = observ_stack_len
      self$env = genv
      self$flag_continous = ifelse(grepl("float", toString(genv$action_space$dtype)), TRUE, FALSE)  # if action is in continous space
      self$name = name
      self$ok_reward = ok_reward
      self$ok_step = ok_step
      self$state_preprocess = state_preprocess$fun
      self$act_cheat = act_cheat
      self$repeat_n_act = repeat_n_act
      private$initActCnt()
      private$initStateDim()
    },

    initSubsample = function() {
      state = self$env$reset()  #FIXME: only return state when reset is called
      hstate = self$state_preprocess(state)
      return(dim(hstate))
    },

    #FIXME: put this function as a user option
    subsample = function(state) {
      I = state[seq(30L, 210L, 3L), seq(1L, 160L, 2L), ]
      I = 0.299 * I[, , 1L] + 0.587 * I[, , 2L] + 0.114 * I[, , 3L]  # RGB to gray formula
      res = array_reshape(I, c(dim(I), 1L))
      res = array(as.integer(res), dim = dim(res))  # store integer is less memory hungry
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
      list_s_r_d_info = lapply(1:self$repeat_n_act, function(i) self$env$step(action)) # repeat the same choice for self$repeat_n_act times. length(list_s_r_d_info) = self$repeat_n_act
      rewards = sapply(list_s_r_d_info, function(x) x[[2L]]) # extract reward for each action repeat: the second element of each s_r_d_info return is reward
      s_r_d_info = list_s_r_d_info[[self$repeat_n_act]]
      names(s_r_d_info) = c("state", "reward", "done", "info")
      s_r_d_info[["reward"]] = sum(rewards)
      s_r_d_info[["state"]] = self$state_preprocess(s_r_d_info[["state"]])  # preprocessing
      if (self$flag_tensor) {
        s_r_d_info[["state"]] = pmax(s_r_d_info[["state"]], private$old_state)  # remove flickering
        private$old_state = s_r_d_info[["state"]]
      }
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
        private$old_state = s
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

    overview = function() {
      cat(sprintf("\naction cnt: %s \n", toString(self$act_cnt)))
      cat(sprintf("state original dim: %s \n", toString(private$old_dim)))
      flag_vec = private$new_dim != private$old_dim
      if (flag_vec[1L]) {
        cat(sprintf("state dim after preprocessing: %s \n", toString(private$new_dim)))
      }
      cat(sprintf("%s\n", ifelse(self$flag_continous, "continous action", "discrete action")))
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
      if (is.null(self$env$action_space$sample)) {
        stop("no support for snapshot for this environment")
      }
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
