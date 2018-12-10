nn4mountainCar = function(name, env) {
  model = keras_model_sequential()
  model %>% layer_dense(units = 10, activation = 'relu', input_shape = c(2)) %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 3, activation = 'linear');model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 9e-4))
  model
}

# instance is the return for problem
rl_algo_dqn = function(data, job, instance) {
  env = makeGymEnv(name = instance)
  agent = initAgent("AgentDQN", env = env)
  if (instance == "MountainCar-v0") {
    model = nn4mountainCar()
    agent$customizeBrain(model)
    agent$updatePara(console = TRUE, render = TRUE,  log = TRUE, policy.maxEpsilon = 0.15, policy.minEpsilon = 0.05, policy.decay = exp(-0.001), replay.batchsize = 10, replay.epochs = 4, agent.lr_decay = exp(-0.001), agent.gamma = 0.95)
  }
    perf = agent$learn(data$iteration)
    return(perf = perf)  # key for table join
}

rl_algo_ddqn = function(data, job, instance) {
  env = makeGymEnv(name = instance)
  agent = initAgent("AgentDDQN", env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}

rl_algo_fdqn = function(data, job, instance) {
  env = makeGymEnv(name = instance)
  agent = initAgent("AgentFDQN", env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}

rl_algo_pg = function(data, job, instance) {
  env = makeGymEnv(name = instance)
  agent = initAgent("AgentPG", env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}

rl_algo_pgb = function(data, job, instance) {
  env = makeGymEnv(name = instance)
  agent = initAgent("AgentPGBaseline", env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}

rl_algo_pgac = function(data, job, instance) {
  env = makeGymEnv(name = instance)
  agent = initAgent("AgentActorCritic", env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}
