#' @import R6
#' @import data.table
#' @import checkmate
#' @import data.table
#' @import reticulate
#' @import keras
#' @import logging
#' @import openssl
#' @import ggplot2
NULL
set.seed(0)

listClass = function(name = NULL) {
  if (is.null(name)) return(c("Agent", "Policy", "ReplayMem"))
  all = getNamespaceExports("rlR")
  mem.idx = which(sapply(all, function(x) grepl(name, x)))
  all[mem.idx]
}

#' @title  Install dependencies
#'
#' @description Install Keras dependencies, if dependencies already installed, will not re-install
#' NULL
#' @return NULL
#' @export
installDep = function() {
  #res <- try({
  #  sess = tf$Session()
  #  hello <- tf$constant('Hello, TensorFlow!')
  #  sess$run(hello)
  #}, silent = TRUE)
  #if (class(res) == "try-error") {
  #  tensorflow::install_tensorflow()
  #}
  res <- try({
    makeAnyModel(input =4, output = 1, list.arch = list.arch)
    }, silent = TRUE)
  if (class(res)[1L] == "try-error") {
    keras::install_keras()
  }
  print("empty return means dependency met")
}

#' @title listAvailAgent
#' @description List all implemented Agents
#' @export
listAvailAgent = function() {
  c("AgentDQN:deep q learning", "AgentFDQN:frozen target deep q learning", "AgentDDQN: double deep q learning", "AgentPG: policy gradient basic", "AgentPGBaseline: policy gradient with baseline", "AgentActorCritic: actor critic method")
}

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

