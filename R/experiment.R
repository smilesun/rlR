#' @title Wrapper for Gym OpenAI environment
#' @description Depends on Gym API definition
#' @param name The name defined in gym
#' @return The wrapped environment
#' @export
makeGymEnv = function(name ="CartPole-v0", ...) {
  gym = import("gym")
  genv = gym$make(name)
  env = EnvGym$new(genv, ...)  # EnvGym is a wrapper to original gym environment
  return(env)
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
  env = makeGymEnv(sname, ...)
  rl.agent = makeAgent(aname, env, conf = conf)
  return(rl.agent$interact)
}
