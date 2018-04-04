#' @examples
#' not run
#' source("rl_h.R")
makeDefaultConfig = function(scenario = "CartPole-v0" ) {
  conf = RLConf$new()
  conf$updatePara("gym", "scenarioname", scenario)
  conf$updatePara("interact", "maxiter", 50L)
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
  interact = makeGymExperiment(conf = conf) 
  perf = interact$run()
  return(perf)
} 

regressionTest_CartPole = function() {  
  conf = makeDefaultConfig("CartPole-v0")
  conf$updatePara("agent", "agentname", "A3C")
  interact = makeGymExperiment(conf = conf) 
  perf = interact$run()
  return(perf)
} 
