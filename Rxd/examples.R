#' @examples
#' not run
#' source("rl_h.R")
makeDefaultConfig = function() {
  RLConf$update("gym", "scenarioname", "MountainCar-v0")
  RLConf$update("interact", "maxiter", 50L)
  RLConf$update("agent", "EPSILON", 0.01)
  RLConf$update("agent", "replayBatchSize", 5L)
  RLConf$update("agent", "memname", "latest")
  RLConf$update("agent", "policy", "epsilonGreedy")
  RLConf$update("nn", "archname", "mountaincar-linear-noreg")
  conf = RLConf$new()
  return(conf)
}

test_gym_pg = function(maxiter = 50L) {
  probe = gymEnvFactory("MountainCar-v0")
  rl.agent = AgentPG$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt)
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
}

# ensure this test case work every time code  is changed
regressiontest_mountaincar_dqn_work = function() {  
  interact = makeGymExperiment(conf = makeDefaultConfig())
  perf = interact$run()
} 

