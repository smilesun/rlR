makeDefaultConfig = function(scenario = "CartPole-v0" ) {
  conf = RLConf$new()
  conf$updatePara("gym", "scenarioname", scenario)
  conf$updatePara("interact", "maxiter", 2L)
  conf$updatePara("agent", "EPSILON", 0.01)
  conf$updatePara("agent", "agentname", "DQN")
  conf$updatePara("agent", "replayBatchSize", 5L)
  conf$updatePara("agent", "memname", "latest")
  conf$updatePara("agent", "policy", "epsilonGreedy")
  conf$updatePara("nn", "archname", "mountaincar-linear-noreg")
  return(conf)
}

regressionTest_DQL = function(name = "CartPole-v0") {  
  conf = makeDefaultConfig(name)
  conf$updatePara("agent", "agentname", "DQL")
  interact = makeGymExperiment(conf = conf) 
  perf = interact$run()
  return(perf)
} 

context("DQL")

test_that("test Cart-Pole with Dql works", {
  regressionTest_DQL()
  expect_true(TRUE)
})


