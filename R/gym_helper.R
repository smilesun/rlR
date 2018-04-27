dqn_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$show()
  conf$updatePara("interact.maxiter", iter)
  conf$updatePara("policy.epsilon", 1)
  conf$updatePara("policy.decay", exp(-0.01))
  conf$updatePara("replay.batchsize", 50L)
  conf$updatePara("interact.beforeActPipe", c("render","epi-step-log"))
  conf$updatePara("agent.nn.arch", list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"
))
  conf$updatePara("policy.name", "policy.epsilonGreedy")


  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf, act.cheat = function(x) {if(x == 1) {return(2)}; x}, actcnt = 2)
  perf = interact$run()
  return(perf)
}

dqn_cart_pole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$updatePara("interact.maxiter", iter)
  conf$updatePara("policy.epsilon", 1)
  conf$updatePara("policy.decay", exp(-0.01))
  conf$updatePara("replay.batchsize", 50L)
  conf$updatePara("interact.beforeActPipe", c("render","epi-step-log"))
  conf$updatePara("agent.nn.arch", list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"
))
  conf$updatePara("policy.name", "policy.epsilonGreedy")

  interact = rlR::makeGymExperiment(name = "CartPole-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

pg_cart_pole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$updatePara("interact.maxiter", iter)
  conf$updatePara("agent.name", "SPG")
  conf$updatePara("policy.name", "policy.predsoftmax")
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

mlr_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$show()
  conf$updatePara("agent.name", "mlr")
  conf$updatePara("interact.maxiter", iter)
  conf$updatePara("interact.render", FALSE)
  conf$updatePara("replay.memname", "latest")
  conf$updatePara("interact.afterStepPipe", c("after.step", "replay.perEpisode.all"))
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf, act.cheat = function(x) {if(x == 1) {return(2)}; x}, actcnt = 2)
  perf = interact$run()
  return(perf)
}

