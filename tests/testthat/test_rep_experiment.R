context("repeat experiment")
test_that("travis repeat experiment", {
  agent.names = c("AgentDQN", "AgentFDQN", "AgentDDQN", "AgentPG", "AgentPGBaseline", "AgentActorCritic")
  env = makeGymEnv("CartPole-v0")
  lapply(agent.names, function(name) repExperiment(sname = "CartPole-v0", aname = name, conf = getDefaultConf(name), nrep = 2, nepi = 2))
  expect_true(TRUE)
})
