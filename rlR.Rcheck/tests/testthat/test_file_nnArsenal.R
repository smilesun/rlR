context("nnArsenal")
test_that("check custom network", {
  fun = function(state_dim, act_cnt) {
    requireNamespace("keras")
    require("keras")
    model = keras_model_sequential()
    model %>%
      layer_dense(units = 256, activation = 'relu', input_shape = c(state_dim)) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 128, activation = 'relu') %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = act_cnt, activation = 'softmax')
    model
  }
  checkCustomNetwork(fun, 3, 3)
  expect_true(TRUE)
})

test_that("default network works", {
  agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)")
  makeKerasModel(input_shape = 2, output_shape = 2, arch.list = agent.nn.arch)
  makeCnnActor(c(32, 32, 3), 10L)
  makeCnnCritic(c(32, 32, 3), 10L)
  #createActorNetwork(3, 2)
  #createCriticNetwork(3, 2)
  expect_true(TRUE)
})


test_that("custom policy network works", {
  conf = getDefaultConf("AgentActorCritic")
  conf$set(console = TRUE)
  env = makeGymEnv("KungFuMaster-ram-v0", repeat_n_act = 4)
  agent = initAgent("AgentActorCritic", env, conf)
  mfun_val = function(state_dim, act_cnt) {
    requireNamespace("keras")
      model = keras::keras_model_sequential()
        model %>% 
          layer_dense(units = 512, activation = "relu",
            input_shape = c(state_dim)) %>%
          layer_dropout(rate = 0.25) %>%
          layer_dense(units = 1,
            activation = "linear")
        model$compile(loss = "mse",
          optimizer = optimizer_rmsprop(lr = 0.001))
        model
    }

  mfun_policy = function(state_dim, act_cnt) {
    requireNamespace("keras")
    model = keras::keras_model_sequential()
      model %>% 
        layer_dense(units = 512, activation = "relu",
          input_shape = c(state_dim)) %>%
        layer_dropout(rate = 0.25) %>%
        layer_dense(units = act_cnt,
          activation = "softmax")
      model$compile(loss = "categorical_crossentropy",
        optimizer = optimizer_rmsprop(lr = 0.001))
      model
}
  agent$customizeBrain(list(value_fun = mfun_val, policy_fun = mfun_policy))
  agent$learn(1L)
  expect_true(TRUE)
})
