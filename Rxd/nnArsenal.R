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
  model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))
  return(model)
}

createModel2 = function(input_shape, output_shape, firstLayer = list(unit = 64, activation ="relu")) {
  model = keras_model_sequential()
  # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim), dim in keras is row major
  model %>% 
    layer_dense(units = firstLayer$unit, activation = firstLayer$activation, input_shape = c(input_shape)) %>%
    layer_dense(units = output_shape, activation = 'linear')
  model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.001))
  return(model)
}



