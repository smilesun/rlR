#' @title AgentFactory
#' 
#' @description
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
AgentFactory = R6Class("AgentFactory")

DQNBuilder = function(actCnt, stateCnt, conf) {   # late evaluation
  AgentDQN$new(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
}

PGBuilder = function(actCnt, stateCnt, conf) {
  AgentPG$new(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
}


AC3Builder = function(actCnt, stateCnt, conf) {
  AgentActorCritic$new(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
}

DQLBuilder = function(actCnt, stateCnt, conf) {
  AgentDDQN$new(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
}

mlrBuilder = function(actCnt, stateCnt, conf) {
  Agentmlr$new(actCnt = actCnt, stateCnt = stateCnt, conf = conf)
}


AgentFactory$static = list(
  "DQN" = DQNBuilder,
  "DDQN" = DQLBuilder, # double Q learning
  "SPG" = PGBuilder,  # simple policy-gradient
  "A3C" = AC3Builder, # actor critic
  "mlr" = mlrBuilder)

AgentFactory$genAgent = function(name) {
  if (name %nin% names(AgentFactory$static)) stop("no such agents")
  return(AgentFactory$static[[name]])
}

