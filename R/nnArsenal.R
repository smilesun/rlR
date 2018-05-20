# keras convention
# input_shape: Dimensionality of the input (integer) not including the samples axis. This argument is required when using this layer as the first layer in a model.
#
# batch_input_shape: Shapes, including the batch size. For instance,
#           ‘batch_input_shape=c(10, 32)’ indicates that the expected
#           input will be batches of 10 32-dimensional vectors.
#           ‘batch_input_shape=list(NULL, 32)’ indicates batches of an
#           arbitrary number of 32-dimensional vectors.
#
# Input and Output Shapes:
#
#      Input shape: nD tensor with shape: ‘(batch_size, ..., input_dim)’.
#      The most common situation would be a 2D input with shape
#      ‘(batch_size, input_dim)’.
#
#      Output shape: nD tensor with shape: ‘(batch_size, ..., units)’.
#      For instance, for a 2D input with shape ‘(batch_size, input_dim)’,
#      the output would have shape ‘(batch_size, unit)’.
#
  #   reshape is not changing the shape, but only changes the filling scheme of the shape
  #'   state = array_reshape(state, c(1L, dim(state))) # fill first the row dimension while in R normally column is filled first
  #'   state = array_reshape(state, c(1L, length(state))) # fill first the row dimension while in R normally column is filled first
  #'   temp = array(1:8, dim = c(2,2,2))
  #'   length(temp) = 8
  #'   Rank: the number of dimensions needed to represent a tensor
  #'   Shape:  c(1,2,3) for rank 3
  #'   "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim), dim in keras is row major
  #'   model = keras_model_sequential()
  #'   model %>%
  #'     layer_dense(units = 64L, activation = 'relu', input_shape = c(input_shape)) %>%
  #'     layer_dense(units = output_shape, activation = 'linear')
  #'   model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))
  #'   return(model)
  #'   model = keras_model_sequential()
  #'   model %>%
  #'     layer_dense(units = 64L, activation = 'relu', input_shape = c(input_shape), kernel_regularizer = regularizer_l2(l = 0.01), bias_regularizer = regularizer_l2(l = 0.1)) %>%
  #'     layer_dense(units = output_shape, activation = 'linear', kernel_regularizer
  #'       = regularizer_l2(l = 0.01), bias_regularizer = regularizer_l2(l = 0.1))
  #'   model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))
  #'   return(model)
  #'
  #' Keras helper:
  #' conf = list(list.par.val = list(list(layer_dense.units = 64, activation_fun = "relu", reg_type = "regularizer_l1", kernel_regularizer = 0.001, bias_regularizer = 0)), loss = "mse", lr = 0.00025 )
  #' a = keras_helper(256, 10, "softmax", conf$loss, conf$lr, conf$list.par.val)
  #' eval(parse(text = a))

makeKerasModel =  function(input_shape =2, output_shape =2, arch.list) {
  nhidden = arch.list$nhidden
  act1 = arch.list$act1
  act2 = arch.list$act2
  loss = arch.list$loss
  lr = arch.list$lr
  kernel_regularizer = arch.list$kernel_regularizer
  bias_regularizer = arch.list$bias_regularizer
  decay = arch.list$decay   # default is 0
  if(is.null(decay)) decay = 0
  clipnorm = arch.list$clipnorm  # default is NULL
  if(is.null(clipnorm)) clipnorm = 1
  expr = sprintf("model = keras_model_sequential();model %%>%%layer_dense(units = %d, activation = '%s', input_shape = c(%d), kernel_regularizer = %s, bias_regularizer = %s) %%>%%layer_dense(units = %d, activation = '%s');model$compile(loss = '%s', optimizer = optimizer_rmsprop(lr = %f, decay = %f, clipnorm = %f)); model", nhidden, act1, input_shape, kernel_regularizer, bias_regularizer, output_shape, act2, loss, lr, decay, clipnorm)
  eval(parse(text = expr))
}

keras_helper = function(input.shape, output.shape, list.arch) {
  lr = list.arch[["lr"]]
  output.act = list.arch[["output.act"]]
  loss = list.arch[["loss"]]
  decay = list.arch[["decay"]]
  clipnorm = list.arch[["clipnorm"]]
  list.par.val = list.arch[["list.par.val"]]
  hh = function(reg_type, val) {
    paste0(reg_type, "(l=", as.character(val), ")")
  }
  s0 = sprintf("model = keras_model_sequential();model ")
  input.shape = input.shape
  list.str = lapply(list.par.val, function(x) {
  sprintf("%%>%%layer_dense(units = %d, activation = '%s', input_shape = c(%d), kernel_regularizer = %s, bias_regularizer = %s)",
    x$layer_dense.units, x$activation_fun, input.shape, hh(x$reg_type, x$kernel_regularizer), hh(x$reg_type, x$bias_regularizer))
  })
  ss = Reduce(paste0, list.str)
  ss = paste0(s0, ss)
  s1 = sprintf("%%>%%layer_dense(units = %d, activation = '%s');", output.shape, output.act)
  ss = paste0(ss, s1)
  s2 = sprintf("model$compile(loss = '%s', optimizer = optimizer_rmsprop(lr = %f, decay = %f, clipnorm = %f)); model", loss, lr, decay, clipnorm)
  ss = paste0(ss, s2)
  ss
}

addLayer = function(...) {
  list(...)
}
makeHidden = function(...) {
  list.arch = list(...)
  return(list.arch)
}

makeArch = function(lr, output.act, loss, decay = 0, clipnorm = 1, list.par.val) {
  return(list(lr = lr, output.act = output.act, loss = loss, decay = decay, clipnorm = clipnorm, list.par.val = list.par.val))
}

list.arch = makeArch(
  lr = 0.025,
  output.act = "linear",
  loss = "mse",
list.par.val = makeHidden(
  addLayer(layer_dense.units = 64, activation_fun = "relu", reg_type = "regularizer_l2", kernel_regularizer = 0, bias_regularizer = 0.1),
  addLayer(layer_dense.units = 32, activation_fun = "softmax", reg_type = "regularizer_l2", kernel_regularizer = 0, bias_regularizer = 0.1)
  )
)

makeAnyModel = function(input =4, output = 1, list.arch) {
  text = keras_helper(input, output, list.arch)
  eval(parse(text = text))
}

#makeAnyModel(list.arch = list.arch)
makeCnn = function(input_shape = c(32, 32, 3), act_cnt = 10L) {
  model <- keras_model_sequential()
  model %>%
    # Start with hidden 2D convolutional layer being fed 32x32 pixel images
    layer_conv_2d(
      filter = 32, kernel_size = c(3,3), padding = "same", 
      input_shape = input_shape
    ) %>%
    layer_activation("relu") %>%

    # Second hidden layer
    layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
    layer_activation("relu") %>%

    # Use max pooling
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.25) %>%
    
    # 2 additional hidden 2D convolutional layers
    layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
    layer_activation("relu") %>%

    # Use max pooling once more
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.25) %>%
    
    # Flatten max filtered output into feature vector 
    # and feed into dense layer
    layer_flatten() %>%
    layer_dense(512) %>%
    layer_activation("relu") %>%
    layer_dropout(0.5) %>%

    # Outputs from dense layer are projected onto 10 unit output layer
    layer_dense(act_cnt) %>%
    layer_activation("softmax")
    opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6)
    model %>% compile(
      loss = "categorical_crossentropy",
      optimizer = opt,
      metrics = "accuracy"
    )
  return(model)
}
