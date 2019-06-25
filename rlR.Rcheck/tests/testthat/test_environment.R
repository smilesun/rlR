context("custom_environment")
test_that("test custom environment EnvToy works", {
  env = EnvToy$new()
  env$overview()
  env$reset()
  env$step(1)
  env$afterAll()
  conf = getDefaultConf("AgentDQN")
  agent = initAgent("AgentDQN", env, conf)
  perf = agent$learn(3L)
  expect_class(perf, "Performance")
})


test_that("Gym constructor Works", {
  env = makeGymEnv(name = "CartPole-v0")
  sr = Surrogate$new(3, c(2, 5, 7), createModel.fun = NULL)
  expect_error(sr$train())
  expect_error(sr$pred())
})
