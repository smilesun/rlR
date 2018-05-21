#' @title Wrapper for Gym OpenAI environment
#' @description Depends on Gym API definition
#' @param name The name defined in gym
#' @return The wrapped environment
#' @export
makeGymEnv = function(name ="CartPole-v0", ...) {
  gym = reticulate::import("gym")
  genv = gym$make(name)
  env = EnvGym$new(genv, ...)  # EnvGym is a wrapper to original gym environment
  return(env)
}


#' @title Make Gym Open AI experiment
#'
#' @description Make Gym Open AI experiment
#'
#' @param sname The scenario name of Gym environment
#' @param aname The name of the Agent
#' @param conf Configuration object
#' @return Interaction object
#' @export
makeGymExperiment = function(sname ="CartPole-v0", aname, conf,  ...) {
  env = makeGymEnv(sname, ...)
  rl.agent = makeAgent(aname, env, conf = conf)
  return(rl.agent$interact)
}
