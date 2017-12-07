# source("rl_h.R")
helper_gym_genEnv = function(name ="CartPole-v0") {
  gym = import("gym")
  genv = gym$make(name)
  genv$reset()
  act_cnt = genv$action_space$n
  state_cnt = genv$observation_space$shape[[1L]]
  env = EnvGym$new(genv)
  return(list(env = env, actCnt = act_cnt, stateCnt = state_cnt))
}


test_gym_pg = function(maxiter = 50L) {
  probe = helper_gym_genEnv("MountainCar-v0")
  rl.agent = AgentPG$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt)
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
}

makeGymExperiment = function(conf = RLConf) {
    probe = helper_gym_genEnv(conf$static$gym$scenarioname)
    rl.agent = AgentFactory$genAgent(conf$static$agent$agentname)(actCnt = probe$actCnt, stateCnt = probe$stateCnt, surro_fun = NNArsenal$makeBrain(RLConf$static$nn$archname))
    interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = conf$static$interact$maxiter)
    return(interact)
  }

  makeGymExperiment2 = function(conf = RLConf) {
    probe = helper_gym_genEnv(conf$static$gym$scenarioname)
    rl.agent = AgentFactory$genAgent(conf$static$agent$agentname)(actCnt = probe$actCnt, stateCnt = probe$stateCnt, surro_fun = NNArsenal$makeBrain(RLConf$static$nn$archname))
    interact = InteractionObserver$new(rl.env = probe$env, rl.agent = rl.agent)
    interact$run()
  }

  test_gym_dqn = function() {
    RLConf$update("gym", "scenarioname", "MountainCar-v0")
    RLConf$update("agent", "EPSILON", 0.4)
    interact = makeGymExperiment()
    interact$run()
  }

