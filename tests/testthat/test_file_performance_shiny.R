context("replay_mem")
test_that("test basic replay_mem works", {
  env = makeGymEnv("CartPole-v1")
  conf = getDefaultConf("AgentDQN")
  conf$set(agent.store.model = T, console = T, render = T)
  agent = makeAgent("AgentDQN", env, conf)
  perf = agent$learn(5)
  perf$startApp()
})


