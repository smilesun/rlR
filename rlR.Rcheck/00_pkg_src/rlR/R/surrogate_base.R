Surrogate = R6::R6Class("Surrogate",
  public = list(
      act_cnt = NULL,
      state_dim = NULL,
      createModel.fun = NULL,
      model = NULL,
    initialize = function(actionCnt, state_dim, createModel.fun) {
      self$act_cnt = actionCnt
      self$state_dim = state_dim
      self$createModel.fun = createModel.fun
    },

    train = function(X_train, Y_train, epochs) {
      stop("not implmented!")
},

    persist = function(path) {
      temp = self$clone()
      save(temp, file = path)
    },

    pred = function(X) {
      stop("not implemented")
    }
    )
)
