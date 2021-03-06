context("gym_basic")

test_that("cran table", {
  agent = initAgent(name = "AgentTable", env = "CliffWalking-v0")
  agent$learn(1)
  expect_true(T)
})

test_that("table", {
  skip_on_cran()
  agent = initAgent(name = "AgentTable", env = "CliffWalking-v0")
  agent$learn(500)
  expect_true(agent$interact$perf$getAccPerf() > -40.0)
})

test_that("cran test initAgent works", {
  agent.names = c("AgentDQN", "AgentFDQN", "AgentDDQN", "AgentPG", "AgentPGBaseline", "AgentActorCritic")
  env = makeGymEnv("CartPole-v0")
  lapply(agent.names, function(name) initAgent(name, env, conf = getDefaultConf(name)))
  expect_true(TRUE)
})

test_that("Basic test Cart-Pole could run with agents", {
  skip_on_cran()
  agent.names = c("AgentDQN", "AgentFDQN", "AgentDDQN", "AgentPG", "AgentPGBaseline", "AgentActorCritic")
  lapply(agent.names, function(agent.name) {
    env = makeGymEnv("CartPole-v0")
    agent = initAgent(agent.name, env)
    agent$learn(1L)
    expect_true(T, info = agent.name)
  })
})

test_that("test Cart-Pole works for each Policy Agent", {
  skip_on_cran()
  agent.names = c("AgentPG", "AgentPGBaseline", "AgentActorCritic")
  lapply(agent.names, function(agent.name) {
    print(agent.name)
    conf = getDefaultConf(agent.name)
    env = makeGymEnv("CartPole-v0")
    agent = initAgent(agent.name, env, conf)
    agent$learn(80)
    expect_true(agent$interact$perf$getAccPerf() > 20, info = agent.name)
  })
})

test_that("test Cart-Pole works for DQN Agent", {
  skip_on_cran()
  env = makeGymEnv("CartPole-v0")
  agent = initAgent("AgentDQN", env)
  agent$learn(100)
  expect_true(agent$interact$perf$getAccPerf() > 20, info = agent.name)
})

test_that("test AgentFDQN works", {
  skip_on_cran()
  skip_on_travis()
  env = makeGymEnv("CartPole-v0")
  agent = initAgent("AgentFDQN", env)
  agent$learn(300)
  ave_reward = agent$interact$perf$getAccPerf()
  expect_true(ave_reward > 20, info = "AgentFDQN")
})


test_that("test AgentDDQN works", {
  skip_on_cran()
  env = makeGymEnv("CartPole-v0")
  agent = initAgent("AgentDDQN", env)
  agent$learn(200)
  expect_true(agent$interact$perf$getAccPerf() > 20, info = agent.name)
})

test_that("test rescue works each Policy based Agent", {
  skip_on_cran()
  agent.names = c("AgentPG", "AgentPGBaseline", "AgentActorCritic")
  lapply(agent.names, function(agent.name) {
    conf = getDefaultConf(agent.name)
    conf$set(agent.flag.reset.net = TRUE)
    env = makeGymEnv("CartPole-v0")
    agent = initAgent(agent.name, env, conf)
    agent$learn(2)
  })
  expect_true(TRUE)
})
