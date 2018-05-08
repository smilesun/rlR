#' @title Wrapper for Gym OpenAI environment
#' @description Depends on Gym API definition
#' @param name The name defined in gym
#' @return The wrapped environment
#' @export 
#' @examples 
#' gymEnvFactory() 
gymEnvFactory = function(name ="CartPole-v0", ...) {
  gym = import("gym")
  genv = gym$make(name)
  env = EnvGym$new(genv, ...)  # EnvGym is a wrapper to original gym environment
  rst = list(env = env, actCnt = env$act_cnt, stateCnt = env$state_cnt)
  return(rst)
}


#' @title Make Gym Open AI experiment
#'
#' @description Make Gym Open AI experiment
#'
#' @param conf value
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
makeGymExperiment = function(sname ="CartPole-v0", aname, conf,  ...) {
  probe = gymEnvFactory(sname, ...)
  rl.agent = AgentFactory$genAgent(aname, actCnt = probe$actCnt, stateCnt = probe$stateCnt, conf = conf)
  interact = InteractionObserver$new(rl.env = probe$env, rl.agent = rl.agent, glogger = rl.agent$glogger, conf = conf)
  return(interact)
}

