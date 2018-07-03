AgentDQN$test = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  conf = rlR.conf.DQN()
  conf$set(console = console, render = render)
  env = makeGymEnv(sname)
  agent = makeAgent("AgentDQN", env, conf)
  agent$learn(iter)
}
AgentDQN$test2 = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  #env = makeGymEnv("MountainCar-v0", act.cheat = function(a) { if(a ==2) return(3); return(a)}, actcnt = 2)
  env = makeGymEnv("MountainCar-v0")
  agent = makeAgent("AgentDQN", env)
  agent$updatePara(console = TRUE, render = TRUE,  log = TRUE, policy.maxEpsilon = 1, policy.minEpsilon = 0.1, policy.decay = exp(-0.01), replay.batchsize = 8, agent.nn.arch = list(nhidden = 8, act1 = "relu", act2 = "linear", loss = "mse", lr = 1e-3, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))
  agent$learn(1000)
}

AgentDQN$test3 = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  env = makeGymEnv("MountainCar-v0")
  agent = makeAgent("AgentDQN", env)
  mfun = function(stateDim, act_cnt) {
    model = keras_model_sequential()
      model %>% layer_dense(units = 10, activation = 'relu', input_shape = c(2)) %>%
        layer_dropout(rate = 0.25) %>%
        layer_dense(units = 3, activation = 'linear');model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 9e-4))
      model
  }
    agent$updatePara(console = TRUE, render = TRUE,  log = TRUE, policy.maxEpsilon = 0.15, policy.minEpsilon = 0.05, policy.decay = exp(-0.001), replay.batchsize = 10, replay.epochs = 4, agent.lr.decay = exp(-0.001), agent.gamma = 0.95)
  agent$customizeBrain(value_fun = mfun)
  agent$learn(1000)
}

AgentDQN$test4 = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  env = makeGymEnv("MountainCar-v0", act.cheat = function(a) { if(a ==2) return(3); return(a)}, actcnt = 2)
  agent = makeAgent("AgentDQN", env)
  model = keras_model_sequential()
  model %>% layer_dense(units = 10, activation = 'relu', input_shape = c(2)) %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 2, activation = 'linear');model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 9e-4))
  model
  agent$updatePara(console = TRUE, render = TRUE,  log = TRUE, policy.maxEpsilon = 0.15, policy.minEpsilon = 0.05, policy.decay = exp(-0.001), replay.batchsize = 10, replay.epochs = 4, agent.lr.decay = exp(-0.001), agent.gamma = 0.95)
  agent$customizeBrain(model)
  agent$learn(1000)
}

AgentDQN$testcnn = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  env = makeGymEnv("Pong-v0", act_cheat = c(2, 3), repeat_n_act = 4)
  agent = makeAgent("AgentDQN", env)
  agent$updatePara(replay.batchsize = 32, render = TRUE, replay.freq = 4L)
  agent$learn(1)
}

AgentDQN$testpongram = function(iter = 1000L, sname = "Bowling-ram-v0", render = FALSE, console = FALSE) {
  #env = makeGymEnv(sname, act_cheat = c(2, 3))
  env = makeGymEnv(sname)
  agent = makeAgent("AgentFDQN", env, rlR.conf.DQN.kungfu())
  agent$learn(iter)
}

AgentDQN$testKungfu = function(iter = 20L, sname = "Kungfu-Master-v0", render = TRUE, console = TRUE) {
  env = makeGymEnv("KungFuMaster-ram-v0")
  agent = makeAgent("AgentDQN", env, rlR.conf.DQN.kungfu())
  agent$updatePara(render = TRUE)
  agent$learn(iter)
}

AgentDQN$testPong = function(iter = 20L, sname = "Breakout-ram-v0", render = TRUE, console = TRUE) {
  env = makeGymEnv(sname)
  agent = makeAgent("AgentFDQN", env, rlR.conf.DQN.kungfu())
  agent$updatePara(render = TRUE)
  agent$learn(iter)
}


rlR.conf.DQN.kungfu = function() {
  RLConf$new(
          render = TRUE,
          console =TRUE,
          log = FALSE,
          policy.maxEpsilon = 1,
          policy.minEpsilon = 0.01,
          policy.decay = exp(-0.001),
          policy.name = "ProbEpsilon",
          replay.batchsize = 64L,
          agent.nn.arch = list(nhidden = 640, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))
}


AgentDDQN$test = function(iter = 1000L, sname = "CartPole-v0", render = TRUE, console = FALSE) {
  conf = rlR.conf.DDQN()
  conf$updatePara("console", console)
  interact = makeGymExperiment(sname = sname, aname = "AgentDDQN", conf = conf, ok_reward = 195, ok_step = 100)
  perf = interact$run(iter)
  return(perf)
}


AgentFDQN$test = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  conf = rlR.conf.FDQN()
  conf$updatePara("console", console)
  conf$updatePara("render", render)
  env = makeGymEnv(sname)
  agent = makeAgent("AgentFDQN", env, conf)
  perf = agent$learn(iter)
}

AgentFDQN$longRunCnn = function(sname = "KungFuMaster-v0", iter = 5000, render = TRUE) {
  conf = getDefaultConf("AgentFDQN")
  #@Note: one episode of pong is around 300 steps
  conf$set(replay.batchsize = 32, replay.freq = 4L, console = TRUE, agent.lr.decay = 1, agent.lr = 0.00025, replay.memname = "UniformStack", render = render, policy.decay = exp(-2.2 / 1e6), policy.minEpsilon = 0.1, agent.start.learn = 5000L, replay.mem.size = 1e6, log = FALSE, agent.update.target.freq = 10000L)
  env = makeGymEnv(sname, repeat_n_act = 4, observ_stack_len = 4L)
  agent = makeAgent("AgentFDQN", env, conf)
  perf = agent$learn(iter)
}


AgentFDQN$testCnn = function(sname = "KungFuMaster-v0", iter = 5000, render = TRUE) {
  conf = getDefaultConf("AgentFDQN")
  #@Note: one episode of pong is around 300 steps
  conf$set(replay.batchsize = 32, replay.freq = 4L, console = TRUE, agent.lr.decay = 1, agent.lr = 0.00025, replay.memname = "UniformStack", render = render, policy.decay = exp(-0.005), policy.minEpsilon = 0.1, agent.start.learn = 50L, replay.mem.size = 1e6, log = FALSE, agent.update.target.freq = 1000L)
  env = makeGymEnv(sname, repeat_n_act = 4, observ_stack_len = 4L)
  agent = makeAgent("AgentFDQN", env, conf)
  perf = agent$learn(iter)
}

AgentPG$test = function(iter = 1000L, sname = "CartPole-v0", render = FALSE) {
  conf = rlR.conf.PG()
  conf$static$render = render
  env = makeGymEnv(sname)
  agent = makeAgent("AgentPG", env, conf)
  agent$learn(iter)
}

AgentPG$testCNN = function(iter = 1000L, sname = "Pong-v0", render = FALSE, console = FALSE) {
  conf = rlR.conf.PG()
  conf$set(console = console, render = render, replay.memname = "Online")
  env = makeGymEnv(sname, repeat_n_act = 40L, act_cheat = c(2L, 3L))
  agent = makeAgent("AgentPG", env, conf)
  agent$learn(iter)
}

AgentActorCritic$test = function(iter = 500L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  set.seed(0)
  conf = rlR.conf.AC()
  conf$updatePara("console", console)
  conf$updatePara("render", render)
  env = makeGymEnv(sname)
  agent = makeAgent("AgentActorCritic", env, conf)
  agent$learn(iter)
}

AgentActorCritic$testKungfu = function(iter = 20L, sname = "Kungfu-Master-v0", render = TRUE, console = TRUE) {
  env = makeGymEnv("KungFuMaster-ram-v0", repeat_n_act = 4)
  agent = makeAgent("AgentActorCritic", env)
  agent$updatePara(render = TRUE)
  agent$learn(iter)
}

AgentPGBaseline$test = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  conf = rlR.conf.PGBaseline()
  conf$updatePara("console", console)
  env = makeGymEnv(sname)
  agent = makeAgent("AgentPGBaseline", env, conf)
  perf = agent$learn(iter)
  perf
}
