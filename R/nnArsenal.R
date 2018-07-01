makeKerasModel =  function(input_shape =2, output_shape =2, arch.list) {
  nhidden = arch.list$nhidden
  act1 = arch.list$act1
  act2 = arch.list$act2
  loss = arch.list$loss
  lr = arch.list$lr
  kernel_regularizer = arch.list$kernel_regularizer
  bias_regularizer = arch.list$bias_regularizer
  decay = arch.list$decay   # default is 0
  if (is.null(decay)) decay = 0
  clipnorm = arch.list$clipnorm  # default is NULL
  if (is.null(clipnorm)) clipnorm = 1
  expr = sprintf("model = keras_model_sequential();model %%>%%layer_dense(units = %d, activation = '%s', input_shape = c(%d), kernel_regularizer = %s, bias_regularizer = %s) %%>%%layer_dense(units = %d, activation = '%s');model$compile(loss = '%s', optimizer = optimizer_rmsprop(lr = %f, decay = %f, clipnorm = %f)); model", nhidden, act1, input_shape, kernel_regularizer, bias_regularizer, output_shape, act2, loss, lr, decay, clipnorm)
  eval(parse(text = expr))
}

makeCnnActor = function(input_shape = c(32, 32, 3), act_cnt = 10L) {
  text = paste("model <- keras_model_sequential();",
  'model %>%',
  ' layer_conv_2d(filter = 2, kernel_size = c(5,5), padding = "same", input_shape = input_shape) %>%',
    'layer_activation("relu") %>%',
    'layer_conv_2d(filter = 2, kernel_size = c(5,5)) %>%',
    'layer_activation("relu") %>%',
    'layer_max_pooling_2d(pool_size = c(2,2)) %>%',
    'layer_dropout(0.25) %>%',
    'layer_conv_2d(filter = 1, kernel_size = c(3,3), padding = "same") %>%',
    'layer_activation("relu") %>%',
    'layer_max_pooling_2d(pool_size = c(2,2)) %>%',
    'layer_dropout(0.25) %>%',
    'layer_flatten() %>%',
    'layer_dense(512) %>%',
    'layer_activation("relu") %>%',
    'layer_dropout(0.5) %>%',
    'layer_dense(act_cnt) %>%',
    'layer_activation("softmax");',
    'opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6);',
    'model %>% compile(loss = "categorical_crossentropy", optimizer = opt, metrics = "accuracy")')
  model = eval(parse(text = text))
  return(model)
}

makeCnnCritic = function(input_shape = c(32, 32, 3), act_cnt = 1L) {
  text = paste("model <- keras_model_sequential();",
  'model %>%',
  ' layer_conv_2d(filter = 1, kernel_size = c(3,3), padding = "same", input_shape = input_shape) %>%',
    'layer_activation("relu") %>%', 'layer_conv_2d(filter = 1, kernel_size = c(3,3)) %>%',
    'layer_activation("relu") %>%',
    'layer_max_pooling_2d(pool_size = c(2,2)) %>%',
    'layer_dropout(0.25) %>%',
    'layer_conv_2d(filter = 1, kernel_size = c(3,3), padding = "same") %>%',
    'layer_activation("relu") %>%',
    'layer_max_pooling_2d(pool_size = c(2,2)) %>%',
    'layer_dropout(0.25) %>%',
    'layer_flatten() %>%',
    'layer_dense(10) %>%',
    'layer_activation("relu") %>%',
    'layer_dropout(0.5) %>%',
    'layer_dense(act_cnt) %>%',
    'layer_activation("softmax");',
    'opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6);',
    'model %>% compile(loss = "mse", optimizer = opt, metrics = "accuracy")')
  model = eval(parse(text = text))
  return(model)
}

createActorNetwork = function(state_dim = 3, action_dim = 1L) {
  input_state = keras::layer_input(shape = state_dim)
  states_hidden = input_state %>%
    layer_dense(units = 27, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = 27, activation = "linear") %>%
    layer_dense(units = 1, activation = "linear")
  model = keras::keras_model(inputs = input_state, outputs = states_hidden2)
  opt = keras::optimizer_adam(lr = 0.0001)
  model %>% compile(
    optimizer = opt,
    loss = "mse",
    metrics = c("accuracy")
    )
  return(list(model = model, input_state = input_state, weights = model$trainable_weights))
}

createCriticNetwork = function(state_dim, action_dim) {
  input_state = keras::layer_input(shape = state_dim)
  input_action = keras::layer_input(shape = action_dim, name = "input_action")
  action_hidden = input_action %>%
    layer_dense(units = 30, activation = "linear")
  states_hidden = input_state %>%
    layer_dense(units = 30, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = 30, activation = "linear")
  hiddens = keras::layer_add(c(states_hidden2, action_hidden))
  # outputs compose input + dense layers
  predictions = hiddens %>%
    layer_dense(units = 30, activation = "relu") %>%
    layer_dense(units = action_dim, activation = "linear")
  # create and compile model
  model = keras::keras_model(inputs = c(input_action, input_state), outputs = predictions)
  opt = keras::optimizer_adam(lr = 0.0001)
  model %>% compile(
    optimizer = opt,
    loss = "mse",
    metrics = c("accuracy")
    )
  return(list(model = model, input_action = input_action, input_state = input_state))
}

checkCustomNetwork = function(fun, input_dim, output_dim) {
  checkmate::assertFunction(fun)
  res = try({
    do.call(fun, args = list(stateDim = input_dim, act_cnt = output_dim))
  })
  checkmate::assertFALSE(class(res)[1L] == "try-error")
  return(fun)
}
