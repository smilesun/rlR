AgentFactory = R6Class("AgentFactory")
AgentFactory$static = list(
  "DQN" = function(actCnt, stateCnt, surro_fun) AgentDQN$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun),
  "DQL" = function(actCnt, stateCnt, surro_fun) AgentDQL$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun),
  "A3C" = function(actCnt, stateCnt, surro_fun) AgentActorCritic$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun)
  )
AgentFactory$genAgent = function(name) {
  if(name %nin% names(AgentFactory$static)) stop("no such agents")
  return(AgentFactory$static[[name]])
}

