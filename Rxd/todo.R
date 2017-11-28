  #  createModel = function(input_shape, output_shape) {
  #      model = keras_model_sequential()
  #      # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim)
  #      # dim in keras is row major
  #      model %>%
  #        layer_dense(units = 256, activation = 'relu', input_shape = c(input_shape)) %>%
  #        layer_dropout(rate = 0.4) %>%
  #        layer_dense(units = 128, activation = 'relu') %>%
  #        layer_dropout(rate = 0.3) %>%
  #        layer_dense(units = output_shape, activation = 'softmax')
  #      model$compile(loss = 'categorical_crossentropy', optimizer = optimizer_rmsprop())
  #      # the only different is that for Policy gradient,the loss must to cross_entropy
  #      return(model)
  #    },


   # createModel = function(input_shape, output_shape) {
   #     model = keras_model_sequential()
   #     # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim)
   #     # dim in keras is row major
   #     model %>%
   #       layer_dense(units = 256, activation = 'relu', input_shape = c(input_shape)) %>%
   #       layer_dropout(rate = 0.4) %>%
   #       layer_dense(units = 128, activation = 'relu') %>%
   #       layer_dropout(rate = 0.3) %>%
   #       layer_dense(units = output_shape, activation = 'softmax')
   #     model$compile(loss = 'mse', optimizer = optimizer_rmsprop())
   #     return(model)
   #   },


