context("gym_continuous")
test_that("test ddpg works", {
  skip_on_cran()
  env = makeGymEnv("Pendulum-v0")
  agent = makeAgent("AgentDDPG", env)
  agent$updatePara(replay.memname = "Online")
  agent$learn(1)
  expect_true(TRUE)
})
