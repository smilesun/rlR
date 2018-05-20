Surrogate = R6Class("Surrogate",
  public = list(
      actCnt = NULL,
      stateCnt = NULL,
      createModel.fun = NULL,
      model = NULL,
    initialize = function(actionCnt, stateCnt, createModel.fun) {
      self$actCnt = actionCnt
      self$stateCnt = stateCnt
      self$model = self$createModel.fun(input_shape = stateCnt, output_shape = actionCnt)  # proxy method
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
