context("configuration")
test_that("Conf object", {
  conf = RLConf$new()
  conf$get("agent.lr")
  conf$updatePara("agent.lr", 0.1)
  conf$set(agent.lr = 0.1)
  conf$ps()
})
