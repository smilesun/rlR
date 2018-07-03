context("configuration")
test_that("Conf object", {
  conf = RLConf$new()
  conf$get("agent.lr")
  conf$updatePara("agent.lr", 0.1)
  conf$set(agent.lr = 0.1)
  conf$ps()
})

context("ddqn")
test_that("ddqn", {
  conf = rlR.conf.DDQN()
  interact = makeGymExperiment(sname = "CartPole-v0", aname = "AgentDDQN", conf = conf, ok_reward = 195, ok_step = 100)
  perf = interact$run(2)
  expect_true(TRUE)
})
