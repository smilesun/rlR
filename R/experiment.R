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
# library(doMC) # registerDoMC(4) # res = repExperiment(sname = "CartPole-v0", aname = "AgentDQN", conf = getDefaultConf("AgentDQN"), nrep = 5, nepi = 200)
repExperiment = function(sname, aname, conf, nrep = 5L, nepi, value_fun = NULL, ...) {
  list.agent = foreach::foreach(i = 1:nrep) %dopar% {
    env = makeGymEnv(sname, ...)
    agent = initAgent(aname, env, conf)
    agent$learn(nepi)
    agent
  }
  list.r = lapply(list.agent, function(agent) {
    agent$interact$perf$list.reward.epi})
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
  return(list(plot = plot, list.r = list.r, list.agent = list.agent))
}
