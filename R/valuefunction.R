# ActionValueNetwork
# keras or mxnet
ActionValueNetwork = R6::R6Class("ActionValueNetwork",
  public = list(
    model = NULL,
    preprocessState = NULL,
    # keras or mxnet model
    initialize = function(model, preprocessState) {
      self$model = model
      self$preprocessState = preprocessState
    },
    predictQ = function(state) {
      state = self$preprocessState(state)
      predict(self$model, state) # another function?
    },
    train = function(state, target) {
      state = self$preprocessState(state)
      keras::fit(self$model, state, target, verbose = 0)
    }
  )
)

# ActionValueTable
ActionValueTable = R6::R6Class("ActionValueTable",
  public = list(
    Q = NULL,
    step.size = NULL,
    # fixme: get number of states and actions automatically from environment
    # fixme: custom initializer, e.g. not to 0
    initialize = function(n.states, n.actions, step.size) {
      self$Q = matrix(0, nrow = n.states, ncol = n.actions)
      self$step.size = step.size
    },
    predictQ = function(state) {
      self$Q[state + 1, ]
    },
    train = function(state, target) {
      self$Q[state + 1, ] = self$Q[state + 1, ] + self$step.size * (target - self$Q[state + 1, ])
    }
  )
)
