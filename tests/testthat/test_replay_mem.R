context("replay_mem")
test_that("test replay_mem works", {
  skip_on_cran()
  conf = rlR.conf.DQN()
  conf$updatePara("replay.memname", "UniformStack")
  env = makeGymEnv("Pong-v0", repeat_n_act = 400, observ_stack_len = 2)
  env$overview()
  expect_true(TRUE)
})

context("custom_environment")
test_that("test inherit_Environment Works", {
  perf = rlR_env_example()
  expect_class(perf, "Performance")
})
