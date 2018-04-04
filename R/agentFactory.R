#' @title AgentFactory
#' 
#' @description
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
AgentFactory = R6Class("AgentFactory")

DQNBuilder = function(actCnt, stateCnt, conf) { 
  AgentDQN$new(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
}

PGBuilder = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger, conf) {
  AgentPG$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger, conf = conf)
}


AC3Builder = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger, conf) {
  AgentActorCritic$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger, conf = conf)
}

DQLBuilder = function(actCnt, stateCnt, surro_fun, memname, policy_fun, glogger, conf) AgentDQL$new(actionCnt = actCnt, stateCnt = stateCnt, surro_fun = surro_fun, memname = memname, policy_fun = policy_fun, glogger = glogger, conf = conf)

AgentFactory$static = list(
  "DQN" = DQNBuilder,
  "DQL" = DQLBuilder,
  "A3C" = AC3Builder)

AgentFactory$genAgent = function(name) {
  if(name %nin% names(AgentFactory$static)) stop("no such agents")
  return(AgentFactory$static[[name]])
}

