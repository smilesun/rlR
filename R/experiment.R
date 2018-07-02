#' @title Wrapper for Gym OpenAI environment
#' @description Depends on Gym API definition
#' @param name The name defined in gym
#' @param ... Other Parameters to pass to EnvGym
#' @return The wrapped environment
#' @export
makeGymEnv = function(name ="CartPole-v0", ...) {
  gspace = reticulate::import("gym.spaces")
  gym = reticulate::import("gym")
  gym$logger$set_level(40)  # supress warning
  gym$logger$setLevel(40)
  genv = gym$make(name)
  env = EnvGym$new(genv, name, ...)  # EnvGym is a wrapper to original gym environment
  return(env)
}


#' @title Make Gym Open AI experiment
#'
#' @description Make Gym Open AI experiment
#'
#' @param sname The scenario name of Gym environment
#' @param aname The name of the Agent
#' @param conf Configuration object
#' @param ... Other Parameters to pass to GymEnv
#' @return Interaction object
#' @export
makeGymExperiment = function(sname ="CartPole-v0", aname, conf = NULL,  ...) {
  env = makeGymEnv(sname, ...)
  rl.agent = makeAgent(aname, env, conf = conf)
  return(rl.agent$interact)
}
