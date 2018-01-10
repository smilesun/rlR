AgentFactory = R6Class("AgentFactory")
AgentFactory$static = list(
  "DQN" = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger) AgentDQN$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger),
  "DQL" = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger) AgentDQL$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger),
  "A3C" = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger) AgentActorCritic$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger)
  )
AgentFactory$genAgent = function(name) {
  if(name %nin% names(AgentFactory$static)) stop("no such agents")
  return(AgentFactory$static[[name]])
}

