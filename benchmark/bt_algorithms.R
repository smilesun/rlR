rl_algo_ddqn = function(data, job, instance, agent.name) {
  env = makeGymEnv(name = instance)
  agent = makeAgent(agent.name, env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}

rl_algo_dqn = function(data, job, instance, agent.name) {
  env = makeGymEnv(name = instance)
  agent = makeAgent(agent.name, env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}

rl_algo_dqn = function(data, job, instance, agent.name) {
  env = makeGymEnv(name = instance)
  agent = makeAgent(agent.name, env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}



mountainCar = function(name, env) {
  agent = makeAgent(name, env)
  model = keras_model_sequential()
  model %>% layer_dense(units = 10, activation = 'relu', input_shape = c(2)) %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 3, activation = 'linear');model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 9e-4))
  model
  agent$updatePara(console = TRUE, render = TRUE,  log = TRUE, policy.maxEpsilon = 0.15, policy.minEpsilon = 0.05, policy.decay = exp(-0.001), replay.batchsize = 10, replay.epochs = 4, agent.lr_decay = exp(-0.001), agent.gamma = 0.95)
  agent$customizeBrain(model)
  agent$learn(1000)
}
