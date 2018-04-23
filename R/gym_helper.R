dqn_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$updatePara("gym", "scenarioname", "MountainCar-v0")
  conf$updatePara("interact", "maxiter", iter)
  interact = rlR::makeGymExperiment(conf = conf)
  perf = interact$run()
  return(perf)
}

dqn_cart_pole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$updatePara("gym", "scenarioname", "CartPole-v0")
  conf$updatePara("interact", "maxiter", iter)
  interact = rlR::makeGymExperiment(conf = conf)
  perf = interact$run()
  return(perf)
}

pg_cart_pole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$updatePara("gym", "scenarioname", "CartPole-v0")
  conf$updatePara("interact", "maxiter", iter)
  conf$updatePara("agent", "agentname", "SPG")
  conf$updatePara("interact", "beforeActPipe", c("render", "epi-step-log"))
  conf$updatePara("interact", "afterStepPipe", c("after", "replayPerEpisode"))
  interact = rlR::makeGymExperiment(conf = conf)
  perf = interact$run()
  return(perf)
}

a3c_cart_pole = function(iter = 2L) {
  conf = rlR::RLConf$new()
  conf$updatePara("gym", "scenarioname", "CartPole-v0")
  conf$updatePara("interact", "maxiter", iter)
  conf$updatePara("agent", "agentname", "A3C")
  conf$updatePara("interact", "beforeActPipe", c("render", "epi-step-log"))
  conf$updatePara("interact", "afterStepPipe", c("after", "replayPerEpisode"))
  interact = rlR::makeGymExperiment(conf = conf)
  perf = interact$run()
  return(perf)
}

