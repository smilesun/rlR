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
      self$state
    },

    step = function(action) {
      self$n.steps = self$n.steps + 1
      # step_: custom step method depending on problem that returns list with
      #   next state, reward, done
      res = private$step_(self, action)
      self$state = res[[1]]
      self$reward = res[[2]]
      self$done = res[[3]]
      list(state = res[[1]], reward = res[[2]], done = res[[3]])
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

# # function to create the corresponding environment
# makeEnvironment = function(step, reset, visualize, gym.name,
#   transitions, rewards, initial.state) {
#
#   if (!missing(step)) {
#     rl.env = Environment$new(step, reset, visualize)
#   }
#   if (!missing(transitions)) {
#     rl.env = MdpEnvironment$new(transitions, rewards, initial.state, visualize)
#   }
#   if (!missing(gym.name)) {
#     rl.env = GymEnvironment$new(gym.name)
#   }
#   rl.env
# }

## step modus
# invisible(readline(prompt="Press [enter] to continue"))
