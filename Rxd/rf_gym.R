# @depend "rf.R", "rf_pg_brain.R", "rf_dqn_agent.R"
# @import "checkmate"


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
  interact$perf$toString()
}


test_gym_dqn = function(maxiter = 50L) {
  probe = helper_gym_genEnv("MountainCar-v0")
  rl.agent = AgentDQN$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt, fun = makeBrain("mountaincar"))
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
  interact$perf$toString()
}

test_gym_dqn2 = function(maxiter = 50L) {
  # Conf = basicConf()
  # conf$update(paramspace, param.name, param.value)
  # conf$update(list())
  probe = helper_gym_genEnv("CartPole-v0")
  rl.agent = AgentDQN$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt, fun = makeBrain("mountaincar"))
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
  interact$perf$toString()
}

