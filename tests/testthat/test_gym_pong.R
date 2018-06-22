context("gym_basic")
test_that("test Cart-Pole works for each Agent", {
  skip_on_cran()
  env = makeGymEnv("Pong-v0")
  agent = makeAgent("AgentPG", env)
  agent$updatePara(replay.memname = "Online")
  agent$learn(1)
  expect_true(TRUE)
})
