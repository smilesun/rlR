# FIXME: epoch is always 1L now
SurroDQN = R6Class("SurroDQN",
  inherit = Surrogate,
  public = list(
    initialize = function(actCnt, stateCnt, fun) {
      self$actCnt = actCnt
      self$stateCnt = stateCnt
      self$createModel = fun
      self$model = self$createModel(input_shape = self$stateCnt, output_shape = self$actCnt)  # proxy method
    },


    train = function(X_train, Y_train, epochs = 1L) {
      fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0)
    },

    pred = function(X) {
      #FIXME: test if pred works for both one instance and instance matrix
      # manipulate the dimension of the X to fit into the correponding surrogate
      res = self$model %>% predict(X)  ## predict.keras.engine.training.Model
      res
    }
    ),
  private = list(),
  active = list()
  )


