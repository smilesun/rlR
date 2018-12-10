context("cnn")
test_that("test cnn stack input works for each value based agent", {
  agent.names = c("AgentDQN", "AgentFDQN", "AgentDDQN")
  lapply(agent.names, function(agent.name) {
    conf = getDefaultConf(agent.name)
    conf$set(replay.batchsize = 32, replay.freq = 40L, console = TRUE, agent.lr.decay = 1, agent.lr = 0.00025, replay.memname = "UniformStack")
    env = makeGymEnv("KungFuMaster-v0", repeat_n_act = 80L, observ_stack_len = 4L)
    agent = initAgent(agent.name, env, conf)
    perf = agent$learn(1)
    expect_class(perf, "Performance")
  })
})

#FIXME:Valueexpected conv2d_46_input to have shape (210, 160, 4) but got array with shape (210, 160, 12)

test_that("test cnn works for each policy based agent", {
  agent.names = c("AgentPG", "AgentPGBaseline", "AgentActorCritic")
  lapply(agent.names, function(agent.name) {
    conf = getDefaultConf(agent.name)
    conf$set(replay.memname = "Online")
    conf$set(replay.batchsize = 32, replay.freq = 40L, console = TRUE, agent.lr.decay = 1, agent.lr = 0.00025)
    env = makeGymEnv("KungFuMaster-v0", repeat_n_act = 80L)
    agent = initAgent(agent.name, env, conf)
    perf = agent$learn(1)
    expect_class(perf, "Performance")
  })
})
