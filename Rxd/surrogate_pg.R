# brain for policy gradient
SurroPG= R6Class("SurroPG",
  inherit = Surrogate,
  public = list(
      createModel = function(input_shape, output_shape) {
        model = keras_model_sequential()
        # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim)
        # dim in keras is row major
        model %>%
          layer_dense(units = 64, activation = 'relu', input_shape = c(input_shape)) %>%
          layer_dense(units = output_shape, activation = 'softmax')
        model$compile(loss = 'categorical_crossentropy', optimizer = optimizer_rmsprop())
        # the only different is that for Policy gradient,the loss must to cross_entropy
        return(model)
      },


      # manipulate the dimension of the X to fit into the correponding surrogate
      train = function(X_train, Y_train, epochs = RLConf$static$EPOCH) {
        # self$model %>% fit(object = self$model, x = X_train, y = Y_train, batch_size = BATCH_SIZE, epochs = EPOCH, verbose = 0, validation_split = VAL_RATE)
        # fit(object = self$model, x = X_train, y = Y_train, batch_size = BATCH_SIZE, epochs = EPOCH, verbose = 0, validation_split = VAL_RATE)
        # fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0, validation_split = VAL_RATE)
        fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0)
    },

    pred = function(X) {
      # return the prediction output for each action
      #FIXME: test if pred works for both one instance and instance matrix
      self$model %>% predict(X)
      ## predict.keras.engine.training.Model
    }
    ),
  private = list(),
  active = list()
  )

SurroPG$test = function() {
  model = PGBrain$new(8, 32)
  model$pred(array(rep(0, 32), dim = c(1L, 32L)))
}
