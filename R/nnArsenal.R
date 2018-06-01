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

makeCnn = function(input_shape = c(32, 32, 3), act_cnt = 10L) {

  model = {
    opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6)
    keras_model_sequential() %>%
      layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = input_shape) %>%
      layer_activation("relu") %>%
      layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
      layer_activation("relu") %>%
      layer_max_pooling_2d(pool_size = c(2,2)) %>%
      layer_dropout(0.25) %>%
      layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same") %>%
      layer_activation("relu") %>%
      layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
      layer_activation("relu") %>%
      layer_max_pooling_2d(pool_size = c(2,2)) %>%
      layer_dropout(0.25) %>%
      layer_flatten() %>%
      layer_dense(512) %>%
      layer_activation("relu") %>%
      layer_dropout(0.5) %>%
      layer_dense(act_cnt) %>%
      layer_activation("softmax") %>%
      compile(loss = "categorical_crossentropy", optimizer = opt, metrics = "accuracy")
  }
  return(model)
}
