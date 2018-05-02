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
  eval(ee)
}

