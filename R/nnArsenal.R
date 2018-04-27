# keras convention
# input_shape: Dimensionality of the input (integer) not including the
#           samples axis. This argument is required when using this layer
#           as the first layer in a model.
# 
# batch_input_shape: Shapes, including the batch size. For instance,
#           ‘batch_input_shape=c(10, 32)’ indicates that the expected
#           input will be batches of 10 32-dimensional vectors.
#           ‘batch_input_shape=list(NULL, 32)’ indicates batches of an
#           arbitrary number of 32-dimensional vectors.
# Input and Output Shapes:
# 
#      Input shape: nD tensor with shape: ‘(batch_size, ..., input_dim)’.
#      The most common situation would be a 2D input with shape
#      ‘(batch_size, input_dim)’.
# 
#      Output shape: nD tensor with shape: ‘(batch_size, ..., units)’.
#      For instance, for a 2D input with shape ‘(batch_size, input_dim)’,
#      the output would have shape ‘(batch_size, unit)’.

  # reshape is not changing the shape, but only changes the filling scheme of the shape
  # state = array_reshape(state, c(1L, dim(state))) # fill first the row dimension while in R normally column is filled first
  # state = array_reshape(state, c(1L, length(state))) # fill first the row dimension while in R normally column is filled first
  # temp = array(1:8, dim = c(2,2,2))
  # length(temp) = 8
  # Rank: the number of dimensions needed to represent a tensor
  # Shape:  c(1,2,3) for rank 3
  # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim), dim in keras is row major
  # model = keras_model_sequential()
  # model %>%
  #   layer_dense(units = 64L, activation = 'relu', input_shape = c(input_shape)) %>%
  #   layer_dense(units = output_shape, activation = 'linear')
  # model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))
  # return(model)
  # model = keras_model_sequential()
  # model %>%
  #   layer_dense(units = 64L, activation = 'relu', input_shape = c(input_shape), kernel_regularizer = regularizer_l2(l = 0.01), bias_regularizer = regularizer_l2(l = 0.1)) %>%
  #   layer_dense(units = output_shape, activation = 'linear', kernel_regularizer
  #     = regularizer_l2(l = 0.01), bias_regularizer = regularizer_l2(l = 0.1))
  # model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))
  # return(model)

makeKerasModel =  function(input_shape =2, output_shape =2, arch.list) {
  nhidden = arch.list$nhidden
  act1 = arch.list$act1
  act2 = arch.list$act2
  loss = arch.list$loss
  lr = arch.list$lr
  kernel_regularizer = arch.list$kernel_regularizer
  bias_regularizer = arch.list$bias_regularizer
  expr = sprintf("model = keras_model_sequential();model %%>%%layer_dense(units = %d, activation = '%s', input_shape = c(%d), kernel_regularizer = %s, bias_regularizer = %s) %%>%%layer_dense(units = %d, activation = '%s');model$compile(loss = '%s', optimizer = optimizer_rmsprop(lr = %f)); model", nhidden, act1, input_shape, kernel_regularizer, bias_regularizer, output_shape, act2, loss, lr)
  eval(parse(text = expr))
}

NNArsenal = R6Class("NNArsenal")

NNArsenal$dqn = function(input_shape, output_shape, ...) {
  arch.list = list(...)[[1]]
  makeKerasModel(input_shape = input_shape, output_shape = output_shape, arch.list)
}

NNArsenal$makeNN4SV = function(input_shape, output_shape = 1L) {
  model = keras_model_sequential()
  model %>%
    layer_dense(units = 64L, activation = 'relu', input_shape = c(input_shape), kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    layer_dense(units = output_shape, activation = 'linear', kernel_regularizer
      = regularizer_l2(l = 0.01), bias_regularizer = regularizer_l2(l = 0.1))
  model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))
  return(model)
}

NNArsenal$makeNN4PG = function(input_shape, output_shape) {
        model = keras_model_sequential()
        # dim in keras is row major
        model %>%
          layer_dense(units = 64, activation = 'relu', input_shape = c(input_shape), kernel_regularizer = regularizer_l2(l = 0.01)) %>%
          layer_dense(units = output_shape, activation = 'softmax')
        model$compile(loss = 'categorical_crossentropy', optimizer = optimizer_rmsprop(lr = 0.025))
        # the only different is that for Policy gradient,the loss must to cross_entropy
        return(model)
}

NNArsenal$makeBrain = function(name) {
  if (name %nin% names(NNArsenal)) stop("no such architecture yet")
  return(NNArsenal[[name]])
}

