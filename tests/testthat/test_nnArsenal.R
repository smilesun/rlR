context("nnArsenal")
test_that("check custom network", {
  fun = function(stateDim, act_cnt) {
    requireNamespace("keras")
    require("keras")
    model = keras_model_sequential()
    model %>%
      layer_dense(units = 256, activation = 'relu', input_shape = c(stateDim)) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 128, activation = 'relu') %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = act_cnt, activation = 'softmax')
    model
  }
  checkCustomNetwork(fun, 3, 3)
})

test_that("default network works", {
  agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)")
  makeKerasModel(input_shape = 2, output_shape = 2, arch.list = agent.nn.arch)
  makeCnnActor(c(32, 32, 3), 10L)
  makeCnnCritic(c(32, 32, 3), 10L)
  createActorNetwork(3, 2)
  createCriticNetwork(3, 2)
})
