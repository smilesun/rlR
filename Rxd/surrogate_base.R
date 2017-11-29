# several base class that do not have too many lines of code
Surrogate= R6Class("Surrogate",
  public = list(
      actionCnt = NULL,
      stateCnt = NULL,
      model = NULL,
    initialize = function(actionCnt, stateCnt) {
      self$actionCnt = actionCnt
      self$stateCnt = stateCnt
      self$model = self$createModel(input_shape = stateCnt, output_shape = actionCnt)  # proxy method
    },

    train = function(X_train, Y_train, epochs = EPOCH){
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


