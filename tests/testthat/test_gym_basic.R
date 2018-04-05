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

# ensure this test case work every time code  is changed
regressionTest_Mountaincar = function() {  
  conf = makeDefaultConfig("MountainCar-v0")
  conf$updatePara("interact", "maxiter", 1L)
  interact = makeGymExperiment(conf = conf) 
  perf = interact$run()
  return(perf)
} 

regressionTest_DQN = function(name = "CartPole-v0") {  
  conf = makeDefaultConfig(name)
  interact = makeGymExperiment(conf = conf) 
  perf = interact$run()
  return(perf)
} 

regressionTest_DQL = function(name = "CartPole-v0") {  
  conf = makeDefaultConfig(name)
  conf$updatePara("agent", "agentname", "DQL")
  interact = makeGymExperiment(conf = conf) 
  perf = interact$run()
  return(perf)
} 

regressionTest_PG = function() {  
  conf = makeDefaultConfig("CartPole-v0")
  conf$updatePara("agent", "agentname", "SPG")
  interact = makeGymExperiment(conf = conf) 
  perf = interact$run()
}

regressionTest_A3C = function() {  
  conf = makeDefaultConfig("CartPole-v0")
  conf$updatePara("agent", "agentname", "A3C")
  interact = makeGymExperiment(conf = conf) 
  perf = interact$run()
}


context("gym_basic")

test_that("test Cart-Pole with DQN works", {
  regressionTest_DQN()
  expect_true(TRUE)
})

test_that("test Cart-Pole with Simple Policy Gradient works", {
  regressionTest_PG()
  expect_true(TRUE)
})

test_that("test Cart-Pole with actor critic works", {
  regressionTest_A3C()
  expect_true(TRUE)
})

test_that("test MountainCar works", {
  # regressionTest_Mountaincar()
  expect_true(TRUE)
})
 
