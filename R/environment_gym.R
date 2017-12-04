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
