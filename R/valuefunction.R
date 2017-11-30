# ActionValueNetwork

# ActionValueTable
ActionValueTable = R6::R6Class("ActionValueTable",
  public = list(
    Q = NULL,
    # get number of states and actions automatically from environment
    # custom initializer, e.g. not to 0
    initialize = function(n.states, n.actions) {
      self$Q = matrix(0, nrow = n.states, ncol = n.actions)
      print("Not implemented")
    },
    predict = function(state) {
      Q[state + 1, ]
    }
  )
)
