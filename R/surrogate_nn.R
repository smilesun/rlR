SurroNN = R6Class("SurroNN",
  inherit = Surrogate,
  public = list(
    initialize = function(actCnt, stateCnt, fun, ...) {
      self$actCnt = actCnt
      self$stateCnt = stateCnt
      self$createModel.fun = fun
      self$model = self$createModel.fun(input_shape = self$stateCnt, output_shape = self$actCnt, ...)  # proxy method
    },

    getWeights = function() {
      keras::get_weights(self$model)
    },

    train = function(X_train, Y_train, epochs = 1L) {
      keras::fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0)
    },

    pred = function(X) {
      res = self$model %>% predict(X)  ## predict.keras.engine.training.Model
      res  # prediction might be NA from Keras
    }
    ),
  private = list(),
  active = list()
  )
