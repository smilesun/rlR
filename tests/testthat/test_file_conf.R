context("configuration")
test_that("Conf object", {
  conf = getDefaultConf("AgentDQN")
  conf$get("agent.lr")
  conf$updatePara("agent.lr", 0.1)
  conf$set(agent.lr = 0.1)
  conf$show()
  expect_true(TRUE)
})

context("naked conf")
test_that("test conf", {
  conf = RLConf$new()
  expect_class(conf, "RLConf")
  RLLog$new(conf)
})
