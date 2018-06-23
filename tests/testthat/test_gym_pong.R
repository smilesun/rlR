context("gym_video")
test_that("test Pong works for each Agent", {
  skip_on_cran()
  env = makeGymEnv("Pong-v0")
  agent = makeAgent("AgentPG", env)
  agent$updatePara(replay.memname = "Online")
  agent$learn(1)
  expect_true(TRUE)
})
