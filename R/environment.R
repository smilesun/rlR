# super class for environment
Environment = R6::R6Class("Environment",
  public = list(
    n.steps = 0,
    state = NULL,
    reward = NULL,
    done = FALSE,

    reset = function() {
      self$n.steps = 0
      self$done = FALSE
      # reset_: custom reset method depending on problem that returns state
      self$state = private$reset_()
    },

    step = function(action) {
      self$n.steps = self$n.steps + 1
      # step_: custom step method depending on problem that returns list with
      #   next state, reward, done
      res = private$step_(self, action)
      self$state = res[[1]]
      self$reward = res[[2]]
      self$done = res[[3]]
    },

    # an optional visualization function
    visualize = function() {
      private$visualize_(self)
    },

    initialize = function(step, reset, visualize) {
      private$step_ = step
      private$reset_ = reset
      if (!missing(visualize)) {
        private$visualize_ = visualize
      }
    }
  ),

  private = list(
    step_ = NULL,
    reset_ = NULL,
    visualize_ = NULL
  )
)

# subclass for MDPs
MdpEnvironment = R6::R6Class("MdpEnvironment",
  inherit = Environment,

  public = list(
    action.space = NULL,
    actions = NULL,
    initial.state = NULL,
    n.actions = NULL,
    n.states = NULL,
    rewards = NULL,
    state.space = NULL,
    states = NULL,
    terminal.states = NULL,
    transitions = NULL,

    initialize = function(transitions, rewards, initial.state, visualize) {
      checkmate::assertArray(transitions, any.missing = FALSE, d = 3)
      checkmate::assertArray(rewards, any.missing = FALSE, min.d = 2, max.d = 2, null.ok = FALSE)
      self$state.space = "Discrete"
      self$action.space = "Discrete"
      self$n.actions = dim(transitions)[3]
      self$n.states = dim(transitions)[1]
      self$actions = seq_len(self$n.actions) - 1L
      self$states = seq_len(self$n.states) - 1L
      self$transitions = transitions
      self$rewards = rewards
      terminal.states = apply(transitions, 3, function(x) diag(x))
      self$terminal.states = which(apply(terminal.states, 1, function(x) all(x == 1))) - 1L
      if (length(self$terminal.states) == 0) {
        warning("There are no terminal states in the MDP!")
        self$terminal.states = -1
      }
      if (missing(initial.state)) {
        self$initial.state = setdiff(self$states, self$terminal.states)
      } else {
        checkmate::assertIntegerish(initial.state, upper = self$n.states - 1)
        self$initial.state = initial.state
      }

      step_ = function(env, action) {
        state = sample(self$states, size = 1,
          prob = self$transitions[self$state + 1, , action + 1])
        reward = self$rewards[state + 1, action + 1]
        if (state %in% self$terminal.states) {
          done = TRUE
        } else {
          done = FALSE
        }
        return(list(state, reward, done))
      }

      reset_ = function() {
        state = ifelse(length(self$initial.state) > 1,
          sample(self$initial.state, size = 1), self$initial.state)
        return(state)
      }
      # call initialize of superclass with mdp step and reset function
      super$initialize(step_, reset_, visualize)
    }
  )
)

# subclass for gym environments
GymEnvironment = R6::R6Class("GymEnvironment",
  inherit = Environment,

  public = list(
    gym.env = NULL,
    gym.name = NULL,

    initialize = function(gym.name) {
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        stop("Please install the reticulate package to use environments from OpenAI Gym.
        Also make sure you have the python package gym installed.",
          call. = FALSE)
      }
      checkmate::assertCharacter(gym.name, len = 1)
      self$gym.name = gym.name

      gym = reticulate::import("gym")
      self$gym.env = gym$make(gym.name)

      step_ = function(env, action) {
        res = self$gym.env$step(action)
        return(res[1:3])
      }

      reset_ = function() {
        state = self$gym.env$reset()
        return(state)
      }

      visualize_ = function(self) {
        self$gym.env$render()
      }

      super$initialize(step_, reset_, visualize_)
    }
  )
)

# function to create the corresponding environment
makeEnvironment = function(step, reset, visualize, gym.name,
  transitions, rewards, initial.state) {

  if (!missing(step)) {
    rl.env = Environment$new(step, reset, visualize)
  }
  if (!missing(transitions)) {
    rl.env = MdpEnvironment$new(transitions, rewards, initial.state, visualize)
  }
  if (!missing(gym.name)) {
    rl.env = GymEnvironment$new(gym.name)
  }
  return(rl.env)
}


# ## Examples ----
# # MDP
# P = array(0, c(2, 2, 2))
# P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
# P[, , 2] = matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
# R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)
# mdp = makeEnvironment(transitions = P, rewards = R)
# mdp$reset()
# mdp$state
# mdp$step(1)
# mdp$state
#
# # Gridworld
# source("R/gridworld.R")
# grid = gridworld(shape = c(4, 4), goal.states = c(0, 15))
# grid$reset()
# grid$state
# grid$visualize()
# grid$step(0L)
# grid$state
# grid$visualize()
#
# # gym
# gym = makeEnvironment(gym.name = "MountainCar-v0")
# gym$reset()
# gym$state
# gym$step(1L)
# gym$state
# for (i in 1:200) {
#   gym$step(sample(0:2, 1))
#   gym$visualize()
# }
# gym$gym.env$close()
#
# # custom user-defined environment
# # Mountain Car example
# # returns state
# reset = function() {
#   position = runif(1, -0.6, -0.4)
#   velocity = 0
#   state = matrix(c(position, velocity), ncol = 2)
#   return(state)
# }
#
# # returns list: state, reward, done
# step = function(env, action) {
#   position = env$state[1]
#   velocity = env$state[2]
#   velocity = velocity + 0.001 * (action - 1) - 0.0025 * cos(3 * position)
#   velocity = min(max(velocity, -0.07), 0.07)
#   position = position + velocity
#   if (position < -1.2) {
#     position = -1.2
#     velocity = 0
#   }
#   state = matrix(c(position, velocity), ncol = 2)
#   reward = -1
#   if (position >= 0.5) {
#     done = TRUE
#     reward = 0
#   } else {
#     done = FALSE
#   }
#   return(list(state, reward, done))
# }
#
# mcar = makeEnvironment(step, reset)
# mcar$reset()
# mcar$state
# mcar$step(1L)
# mcar$state
#
# ## step modus
# # invisible(readline(prompt="Press [enter] to continue"))
