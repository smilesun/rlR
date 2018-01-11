#' @examples
#' not run
#' source("rl_h.R")

test_gym_pg = function(maxiter = 50L) {
  probe = gymEnvFactory("MountainCar-v0")
  rl.agent = AgentPG$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt)
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
}


test_gym_dqn = function() {
# RLConf$update("agent", "agentname", "A3C")
  RLConf$update("gym", "scenarioname", "MountainCar-v0")
  RLConf$update("interact", "maxiter", 50L)
  RLConf$update("agent", "EPSILON", 0.01)
  RLConf$update("agent", "replayBatchSize", 5L)
  RLConf$update("agent", "memname", "latestprob")
  RLConf$update("agent", "policy", "softmax")
  RLConf$update("nn", "archname", "mountaincar-linear-noreg")

  conf = RLConf$new()
  interact = makeGymExperiment(conf = conf)
  #interact = makeGymExperimentObserver()
  perf = interact$run()
  browser()
}

# always make this test case work every time you  change the code 
regressiontest_mountaincar_dqn_work = function() {  
  RLConf$update("gym", "scenarioname", "MountainCar-v0")
  RLConf$update("interact", "maxiter", 50L)
  RLConf$update("agent", "EPSILON", 0.01)
  RLConf$update("agent", "replayBatchSize", 5L)
  RLConf$update("agent", "memname", "latest")
  RLConf$update("agent", "policy", "epsilonGreedy")
  RLConf$update("nn", "archname", "mountaincar-linear-noreg")
  conf = RLConf$new()
  interact = makeGymExperiment(conf = conf)
  perf = interact$run()
  browser()
} 
