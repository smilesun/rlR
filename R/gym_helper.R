dqn_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$set(interact.maxiter = iter,
           agent.name = "AgentDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.1),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 25L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

ddqn_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$show()
  conf$set(interact.maxiter = iter,
           agent.name = "AgentDDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.1),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 50L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

fdqn_mountain = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$show()
  conf$set(interact.maxiter = iter,
           agent.name = "AgentFDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.2),
           policy.name = "policy.epsilonGreedy",
           replay.memname = "latest",
           replay.batchsize = 10L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

fdqn_cart = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$show()
  conf$set(interact.maxiter = iter,
           agent.name = "AgentFDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.1),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 50L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           agent.nn.arch = list(nhidden = 64, act1 = "linear", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "CartPole-v0", conf = conf)
  perf = interact$run()
  return(perf)
}


dqn_atari = function(iter = 1L) {
  names = c("Amidar-ram-v0", "WizardOfWor-ram-v0", "Asteroids-ram-v0", "KungFuMaster-ram-v0", "JourneyEscape-ram-v-ram-v0")
  conf = rlR::RLConf$new()
  conf$set(interact.maxiter = iter,
           agent.name = "AgentDDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 5L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  conf$show()
  #interact = rlR::makeGymExperiment(name = "KungFuMaster-ram-v0", conf = conf)
  interact = rlR::makeGymExperiment(name = "Humanoid-v2", conf = conf)
  perf = interact$run()
  return(perf)
}


dqn_cart_pole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$set(interact.maxiter = iter,
           agent.name = "AgentDDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 50L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "CartPole-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

pg_cart_pole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$set(interact.maxiter = iter,
           agent.name = "AgentPG",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.predsoftmax",
           replay.batchsize = 50L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           interact.afterStepPipe = c("after.step", "replayPerEpisode"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "CartPole-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

a3c_cart_pole = function(iter = 2L) {
  conf = rlR::RLConf$new()
  conf$set(interact.maxiter = iter,
           agent.name = "AgentActorCritic",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.predsoftmax",
           replay.batchsize = 50L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           interact.afterStepPipe = c("after.step", "replayPerEpisode"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name ="CartPole-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

a3c_atari = function(iter = 2L) {
  conf = rlR::RLConf$new()
  conf$set(interact.maxiter = iter,
          agent.name = "AgentActorCritic",
          policy.epsilon = 1,
          policy.decay = exp(-0.01),
          policy.name = "policy.predsoftmax",
          replay.batchsize = 50L,
          interact.beforeActPipe = c("render", "epi-step-log"),
          interact.afterStepPipe = c("after.step", "replayPerEpisode"),
          agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name ="KungFuMaster-ram-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

mlr_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$show()
  conf$set(interact.maxiter = iter,
           agent.name = "Agentmlr",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.predsoftmax",
           replay.batchsize = 50L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           interact.afterStepPipe = c("after.step", "replay.perEpisode.all"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

mlr_cartepole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$show()
  conf$set(interact.maxiter = iter,
           agent.name = "Agentmlr",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.predsoftmax",
           replay.batchsize = 50L,
           interact.beforeActPipe = c("render", "epi-step-log"),
           interact.afterStepPipe = c("after.step", "replay.perEpisode.all"),
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "CartPole-v0", conf = conf)
  perf = interact$run()
  return(perf)
}

