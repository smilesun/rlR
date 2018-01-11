AgentFactory = R6Class("AgentFactory")
AgentFactory$static = list(
  "DQN" = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger, conf) AgentDQN$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger, conf = conf),
  "DQL" = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger, conf) AgentDQL$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger, conf = conf),
  "A3C" = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger, conf) AgentActorCritic$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger, conf = conf)
  )
AgentFactory$genAgent = function(name) {
  if(name %nin% names(AgentFactory$static)) stop("no such agents")
  return(AgentFactory$static[[name]])
}

