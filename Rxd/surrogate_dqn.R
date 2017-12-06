SurroDQN = R6Class("SurroDQN",
  inherit = Surrogate,
  public = list(
    actionCnt = NULL,
    stateCnt = NULL,
    model = NULL,
    createModel = NULL,
    initialize = function(actionCnt, stateCnt, fun = createModel_mountainCar) {
      self$actionCnt = actionCnt
      self$stateCnt = stateCnt
      self$createModel = fun
      self$model = self$createModel(input_shape = stateCnt, output_shape = actionCnt)  # proxy method
    },


    train = function(X_train, Y_train, epochs = RLConf$static$EPOCH) {
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


