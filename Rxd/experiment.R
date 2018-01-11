gymEnvFactory = function(name ="CartPole-v0") {
  gym = import("gym")
  genv = gym$make(name)
  genv$reset()
  act_cnt = genv$action_space$n
  state_cnt = genv$observation_space$shape[[1L]]
  env = EnvGym$new(genv)
  mmap = list("MountainCar-v0"=c(0, 2)) # omit the 1 action which is doing nothing, mapping 1 to 2
  act_cnt_reduced = length(mmap[[name]])
  return(list(env = env, actCnt = act_cnt, stateCnt = state_cnt, decorator = function(x){mmap[[name]][x]}, act_cnt_reduced))
}


makeGymExperiment = function(conf) {
  glogger = RLLog$new(conf)
  probe = gymEnvFactory(conf$static$gym$scenarioname)
  rl.agent = AgentFactory$genAgent(conf$static$agent$agentname)(actCnt = probe$actCnt, stateCnt = probe$stateCnt, surro_fun = NNArsenal$makeBrain(RLConf$static$nn$archname), memname = RLConf$static$agent$memname, policy_fun = conf$static$agent$policy, glogger = glogger, conf = conf)
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = conf$static$interact$maxiter, glogger = glogger, conf = conf)
  return(interact)
  }

makeGymExperimentObserver = function(conf = RLConf) {
    probe = gymEnvFactory(conf$static$gym$scenarioname)
    rl.agent = AgentFactory$genAgent(conf$static$agent$agentname)(actCnt = probe$actCnt, stateCnt = probe$stateCnt, surro_fun = NNArsenal$makeBrain(RLConf$static$nn$archname))
    interact = InteractionObserver$new(rl.env = probe$env, rl.agent = rl.agent)
    return(interact)
  }

