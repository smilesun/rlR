dqn_mountain_car = function(iter = 500L) {
  conf = rlR::RLConf$new(
           policy.epsilon = 1,
           policy.decay = exp(-0.001),
           policy.name = "EpsilonGreedy",
           replay.memname = "Uniform",
           replay.batchsize = 60L,
           agent.nn.arch = list(nhidden = 64, act1 = "sigmoid", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0)", bias_regularizer = "regularizer_l2(l=0)")
           )
  interact = rlR::makeGymExperiment(sname = "MountainCar-v0", aname = "AgentDQN", conf = conf, actcnt = 2L, act.cheat = function(x) {
  ifelse(x == 1L, 0L, 2L)  # we do not need the stay action, this is a way to cheat but it makes learning much faster.
           })
  perf = interact$run(iter)
  return(perf)
}

ddqn_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new(
           agent.name = "AgentDDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.05),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "linear", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}

ddqn_cartpole = function(iter = 1L) {
  conf = rlR::RLConf$new(
           agent.name = "AgentDDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.05),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "CartPole-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}


fdqn_mountain = function(iter = 1L) {
  conf = rlR::RLConf$new(
           agent.name = "AgentFDQN",
           policy.epsilon = 1,
           policy.minEpsilon = 0.01,
           policy.decay = exp(-0.2),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "sigmoid", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}

fdqn_atari = function(iter = 1L) {
  conf = rlR::RLConf$new(
           agent.name = "AgentFDQN",
           policy.epsilon = 1,
           policy.minEpsilon = 0,
           policy.decay = exp(-0.05),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "linear", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "KungFuMaster-ram-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}




dqn_atari = function(iter = 1L) {
  names = c("Amidar-ram-v0", "WizardOfWor-ram-v0", "Asteroids-ram-v0", "KungFuMaster-ram-v0", "JourneyEscape-ram-v-ram-v0")
  conf = rlR::RLConf$new(
           agent.name = "AgentDDQN",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.epsilonGreedy",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "KungFuMaster-ram-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}






a3c_mount = function(iter = 2L) {
  conf = rlR::RLConf$new(
           agent.name = "AgentActorCritic",
           policy.name = "policy.epsilonGreedy",
           replay.memname = "Latest",
           replay.batchsize = 5L,
           agent.nn.arch = list(nhidden = 64, act1 = "sigmoid", act2 = "softmax", loss = "categorical_crossentropy", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0)", bias_regularizer = "regularizer_l2(l=0.0)"),
          agent.nn.arch.critic = list(nhidden = 64, act1 = "sigmoid", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)")
           )
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}


a3c_atari = function(iter = 2L) {
  conf = rlR::RLConf$new()
  conf$set(
          agent.name = "AgentActorCritic",
          policy.name = "policy.epsilonGreedy",
          replay.memname = "Latest",
          agent.nn.arch = list(nhidden = 64, act1 = "sigmoid", act2 = "softmax", loss = "categorical_crossentropy", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"),
          agent.nn.arch.critic = list(nhidden = 64, act1 = "sigmoid", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)")
          )
  interact = rlR::makeGymExperiment(name = "KungFuMaster-ram-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}

mlr_mountain_car = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$set(interact.maxiter = iter,
           agent.name = "Agentmlr",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.predsoftmax",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "MountainCar-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}

mlr_cartepole = function(iter = 1L) {
  conf = rlR::RLConf$new()
  conf$set(interact.maxiter = iter,
           agent.name = "Agentmlr",
           policy.epsilon = 1,
           policy.decay = exp(-0.01),
           policy.name = "policy.predsoftmax",
           replay.batchsize = 50L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))
  interact = rlR::makeGymExperiment(name = "CartPole-v0", conf = conf)
  perf = interact$run(iter)
  return(perf)
}



