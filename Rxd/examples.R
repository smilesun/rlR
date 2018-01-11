# source("rl_h.R")
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


test_gym_pg = function(maxiter = 50L) {
  probe = gymEnvFactory("MountainCar-v0")
  rl.agent = AgentPG$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt)
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
}

makeGymExperiment = function(conf = RLConf, glogger) {
    probe = gymEnvFactory(conf$static$gym$scenarioname)
    rl.agent = AgentFactory$genAgent(conf$static$agent$agentname)(actCnt = probe$actCnt, stateCnt = probe$stateCnt, surro_fun = NNArsenal$makeBrain(RLConf$static$nn$archname), memname = RLConf$static$agent$memname, policy_fun = conf$static$agent$policy, glogger = glogger)
    interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = conf$static$interact$maxiter, glogger = glogger)
    return(interact)
  }

  makeGymExperimentObserver = function(conf = RLConf) {
    probe = gymEnvFactory(conf$static$gym$scenarioname)
    rl.agent = AgentFactory$genAgent(conf$static$agent$agentname)(actCnt = probe$actCnt, stateCnt = probe$stateCnt, surro_fun = NNArsenal$makeBrain(RLConf$static$nn$archname))
    interact = InteractionObserver$new(rl.env = probe$env, rl.agent = rl.agent)
    return(interact)
  }

  test_gym_dqn = function() {
    # RLConf$update("agent", "agentname", "A3C")
    RLConf$update("gym", "scenarioname", "MountainCar-v0")
    RLConf$update("interact", "maxiter", 50L)
    RLConf$update("agent", "EPSILON", 0.01)
    RLConf$update("agent", "replayBatchSize", 5L)
    RLConf$update("agent", "memname", "latestprob")
    RLConf$update("agent", "policy", "softmax")
    RLConf$update("nn", "archname", "mountaincar-linear-noreg")
    # RLConf$update("nn", "archname", "mountaincar-linear-reg")
    glogger = RLLog$new(RLConf)
    interact = makeGymExperiment(glogger = glogger)
    #interact = makeGymExperimentObserver()
    perf = interact$run()
    browser()
    # save(perf, file = "perf.RData")
  }

  test_mountaincar_dqn_work = function() {  # always make this test case work every time you  change the code 
    # RLConf$update("agent", "agentname", "A3C")
    RLConf$update("gym", "scenarioname", "MountainCar-v0")
    RLConf$update("interact", "maxiter", 50L)
    RLConf$update("agent", "EPSILON", 0.01)
    RLConf$update("agent", "replayBatchSize", 5L)
    RLConf$update("agent", "memname", "latest")
    RLConf$update("agent", "policy", "epsilonGreedy")
    RLConf$update("nn", "archname", "mountaincar-linear-noreg")
    glogger = RLLog$new(RLConf)
    interact = makeGymExperiment(glogger = glogger)
    #interact = makeGymExperimentObserver()
    perf = interact$run()
    browser()
    # save(perf, file = "perf.RData")
  }

