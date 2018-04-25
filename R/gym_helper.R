dqn_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$show()
  conf$updatePara("interact.maxiter", iter)
  conf$updatePara("interact.render", FALSE)
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf, act.cheat = function(x) {if(x == 1) {return(2)}; x}, actcnt = 2)
  perf = interact$run()
  return(perf)
}

dqn_cart_pole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$updatePara("interact.maxiter", iter)
  conf$updatePara("interact.beforeActPipe", "epi-step-log")
  conf$updatePara("policy.name", "policy.epsilonGreedy")
  conf$show()
  interact = rlR::makeGymExperiment(name = "CartPole-v0", conf = conf, state.cheat = function(x) x[3:4])
  perf = interact$run()
  return(perf)
}

pg_cart_pole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$updatePara("interact.maxiter", iter)
  conf$updatePara("agent.name", "SPG")
  conf$updatePara("interact.beforeActPipe", c("render", "epi-step-log"))
  conf$updatePara("interact.afterStepPipe", c("after.step", "replayPerEpisode"))
  interact = rlR::makeGymExperiment(name ="CartPole-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

a3c_cart_pole = function(iter = 2L) {
  conf = rlR::RLConf$new()
  conf$updatePara("interact.scenarioname", "CartPole-v0")
  conf$updatePara("interact.maxiter", iter)
  conf$updatePara("agent.name", "A3C")
  conf$updatePara("interact.beforeActPipe", c("render", "epi-step-log"))
  conf$updatePara("interact.afterStepPipe", c("after.step", "replayPerEpisode"))
  interact = rlR::makeGymExperiment(name ="CartPole-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

