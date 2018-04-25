# FIXME: some gym environment have actions that does not make sense
gymActionRemap = function() {
  mmap = list("MountainCar-v0"=c(0, 2))  # omit the 1 action which is doing nothing, mapping 1 to 2
  decorator = function(x){mmap[[name]][x]}
  act_cnt_reduced = length(mmap[[name]])
}

#' @title Wrapper for Gym OpenAI environment
#' @description Depends on Gym API definition
#' @param name The name defined in gym
#' @return The wrapped environment
#' @export 
#' @examples 
#' gymEnvFactory() 
gymEnvFactory = function(name ="CartPole-v0") {
  gym = import("gym")
  genv = gym$make(name)
  genv$reset()
  act_cnt = genv$action_space$n   # get the number of actions/control bits
  state_cnt = genv$observation_space$shape[[1L]]  # get the number of state variables
  env = EnvGym$new(genv)  # EnvGym is a wrapper to original gym environment
  rst = list(env = env, actCnt = act_cnt, stateCnt = state_cnt)
  return(rst)
}


#' @title 
#' 
#' @description
#' 
#' @param conf value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
makeGymExperiment = function(conf) {
  probe = gymEnvFactory(conf$get("interact.scenarioname"))
  rl.agent = AgentFactory$genAgent(conf$get("agent.name"))(actCnt = probe$actCnt, stateCnt = probe$stateCnt, conf = conf)
  interact = InteractionObserver$new(rl.env = probe$env, rl.agent = rl.agent, glogger = rl.agent$glogger, conf = conf) 
  return(interact)
}

