Surrogate = R6Class("Surrogate",
  public = list(
      actCnt = NULL,
      stateCnt = NULL,
      stateDim = NULL,
      createModel.fun = NULL,
      model = NULL,
    initialize = function(actionCnt, stateDim, createModel.fun) {
      self$actCnt = actionCnt
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
    ),
  private = list(),
  active = list()
  )
