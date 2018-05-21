#' @title Make Agent
#' @description Make Agent
#' @param name The name of the Agent
#' @param env The environment to initialize the Agent
#' @param conf The configuration
#' @return [\code{\link{AgentArmed}}].
#' @export
makeAgent = function(name, env, conf = NULL) {
  ee = parse(text = sprintf("%s$new(env = env, conf = conf)", name))
  agent = eval(ee)  # the text is with respect to the passed arguments
  agent
}
