context("gym_continuous")
test_that("test ddpg works", {
  skip_on_cran()
  env = makeGymEnv("Pendulum-v0")
  conf = getDefaultConf("AgentDQN")
  agent = initAgent("AgentDDPG", env, conf)
  agent$learn(1)
  expect_true(T)
})
