#' @title AgentFactory
#' 
#' @description AgentFactory
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
AgentFactory = R6Class("AgentFactory")
AgentFactory$genAgent = function(name, actCnt, stateCnt = stateCnt, conf = conf) {
  ee = parse(text = sprintf("%s$new(actCnt = actCnt, stateCnt = stateCnt, conf = conf)", name))
  eval(ee)  # the text is with respect to the passed arguments
}

#' @title Make Agent
#' @description Make Agent
#' @param name The name of the Agent
#' @param actCnt The number of actions
#' @param stateCnt The size of the state space
#' @param conf The configuration
#' @return AgentArmed
#' @export
#' @examples
#' makeAgent("AgentDQN", actCnt = 2, stateCnt = 2)
makeAgent = function(name, env, conf = NULL) {
  actCnt = env$act_cnt
  stateCnt = env$state_cnt
  ee = parse(text = sprintf("%s$new(actCnt = actCnt, stateCnt = stateCnt, conf = conf)", name))
  agent = eval(ee)  # the text is with respect to the passed arguments
  agent$setInteract(env)
  agent
}
