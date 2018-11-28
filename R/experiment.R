#' @title Wrapper for Gym OpenAI environment
#' @description Depends on Gym API definition
#' @param name The name defined in gym
#' @param ... Other Parameters to pass to EnvGym
#' @return The wrapped environment
#' @export
makeGymEnv = function(name ="CartPole-v0", ...) {
  gspace = reticulate::import("gym.spaces", delay_load = TRUE)
  gym = reticulate::import("gym", delay_load = TRUE)
  gym$logger$set_level(40)  # supress warning
  gym$logger$setLevel(40)
  genv = gym$make(name)
  env = EnvGym$new(genv, name, ...)  # EnvGym is a wrapper to original gym environment
  return(env)
}


#' @title Repeat experiment
#'
#' @description Repeat the experiment for serveral times
#'
#' @param sname The scenario name of Gym environment
#' @param aname The name of the Agent
#' @param conf Configuration object
#' @param nrep Number of repetitions
#' @param nepi Number of episode to learn
#' @param ... Other Parameters to pass to GymEnv
#' @return ggplot2 object
#' @export
# res = repExperiment(sname = "CartPole-v0", aname = "AgentFDQN", conf = getDefaultConf("AgentFDQN"), nrep = 5, nepi = 200)
repExperiment = function(sname, aname, conf, nrep = 3L, nepi, ...) {
  requireNamespace(foreach)
  list.res = foreach::foreach(i = 1:nrep) %dopar% {
    env = makeGymEnv(sname, ...)
    rl.agent = makeAgent(aname, env, conf = conf)
    perf = rl.agent$learn(nepi)
    list(agent = rl.agent, perf = perf)
  }
  list.r = lapply(list.res, function(res) {
    res$perf$list.reward.epi})
  init = list.r[[1L]]
  agent = list.res[[1L]]$agent
  len = agent$env$maxStepPerEpisode
  convert2SameLen = function(init1) {
    init2 = vector(mode = "numeric", length = len)
    init2[1:length(init1)] = init1
    init2
  }
  for (i in 2:length(list.r)) {
    init = lapply(1:length(init), function(j) convert2SameLen(init[[j]]) + convert2SameLen(list.r[[i]][[j]]))
  }
  init = lapply(init, function(vec) vec/nrep)
  agent$interact$perf$list.reward.epi = init
  plot = agent$plotPerf()
  return(list(plot = plot, list.r = list.r))
}
