context("cnn")
test_that("test cnn stack input works", {
  conf = getDefaultConf("AgentFDQN")
  conf$set(replay.batchsize = 32, replay.freq = 4L, console = TRUE, agent.lr.decay = 1, agent.lr = 0.00025, replay.memname = "UniformStack")
  env = makeGymEnv("Pong-v0", act_cheat = c(2, 3), repeat_n_act = 4, observ_stack_len = 4L)
  agent = makeAgent("AgentFDQN", env, conf)
  perf = agent$learn(1)
  expect_class(perf, "Performance")
})
