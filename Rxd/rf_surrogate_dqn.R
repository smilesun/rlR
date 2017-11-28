makeBrain = function(name) {
  if(name =="mountaincar") return(createModel_mountainCar)
  else stop("no further nn architecture available from this function")
}

createModel_mountainCar = function(input_shape, output_shape) {
  model = keras_model_sequential()
  # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim), dim in keras is row major
  model %>%
    layer_dense(units = 64L, activation = 'relu', input_shape = c(input_shape)) %>%
    layer_dense(units = output_shape, activation = 'linear')
  model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.00025))
  return(model)
}


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

    createModel2 = function(input_shape, output_shape, firstLayer = list(unit = 64, activation ="relu")) {
      model = keras_model_sequential()
      # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim), dim in keras is row major
      model %>%
        layer_dense(units = firstLayer$unit, activation = firstLayer$activation, input_shape = c(input_shape)) %>%
        layer_dense(units = output_shape, activation = 'linear')
      model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.001))
      return(model)
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


