rl_algo = function(data, job, instance, agent.name) {
  env = makeGymEnv(name = instance)
  agent = makeAgent(agent.name, env = env)
  perf = agent$learn(data$iteration)
  return(perf = perf)  # key for table join
}
