Surrogate = R6::R6Class("Surrogate",
  public = list(
      act_cnt = NULL,
      stateDim = NULL,
      createModel.fun = NULL,
      model = NULL,
    initialize = function(actionCnt, stateDim, createModel.fun) {
      self$act_cnt = actionCnt
      self$stateDim = stateDim
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
