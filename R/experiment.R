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
#' @param value_fun customized neural network as value function approximator, default NULL
#' @param ... Other Parameters to pass to GymEnv
#' @return list of ggplot2 object for performance and list of reward per experiment per episode
#' @export
# library(doMC)
# registerDoMC(4)
# res = repExperiment(sname = "CartPole-v0", aname = "AgentDQN", conf = getDefaultConf("AgentDQN"), nrep = 5, nepi = 200)
repExperiment = function(sname, aname, conf, nrep = 5L, nepi, value_fun = NULL, ...) {
  list.res = foreach::foreach(i = 1:nrep) %dopar% {
    env = makeGymEnv(sname, ...)
    rl.agent = initAgent(aname, env, conf = conf)
    if (is.function(value_fun)) rl.agent$customizeBrain(value_fun = value_fun)
    perf = rl.agent$learn(nepi)
    list(agent = rl.agent, perf = perf)
  }
  list.agent = lapply(list.res, function(res) res$agent)
  list.r = lapply(list.res, function(res) {
    res$perf$list.reward.epi})
  list.len = lapply(1:nrep, function(i) lapply(list.r[[i]], function(x) length(x)))
  len = max(unlist(list.len))
  init.list = lapply(1:nepi, function(j) vector(mode = "numeric", length = len))
  convert2SameLen = function(init1) {
    init2 = vector(mode = "numeric", length = len)
    init2[1:length(init1)] = init1
    init2
  }
  list.episode = lapply(1:nepi, function(episode_ind) {
    init = vector(mode = "numeric", length = len)
    for (i in 1:nrep) {
      init = init + convert2SameLen(list.r[[i]][[episode_ind]])
    }
    init
  })
  #for (i in 1L:nrep) {
  #  init.list = lapply(2:nepi, function(episode_ind) init.list[[episode_ind]] + convert2SameLen(list.r[[i]][[episode_ind]]))
  #}
  #init = lapply(init, function(vec) vec / nrep)
  #list.episode = lapply(init, function(vec) vec / nrep)
  list.episode = lapply(list.episode, function(vec) vec / nrep)
  env = makeGymEnv(sname, ...)
  agent = initAgent(aname, env, conf = conf)
  #agent$interact$perf$list.reward.epi = init
  agent$interact$perf$list.reward.epi = list.episode
  plot = agent$plotPerf()
  return(list(plot = plot, list.r = list.r, agent.list = agent.list))
}
