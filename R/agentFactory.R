#' @title Create Agent R6 class
#' @description A factory method for creating different learning agent
#' @param name The name of the Agent
#' @param env The environment to initialize the Agent
#' @param conf The configuration
#' @return [\code{\link{AgentArmed}}].
#' @examples makeAgent("AgentDQN", makeGymEnv(name ="CartPole-v0"), getDefaultConf("AgentDQN"))
#' @export
makeAgent = function(name, env, conf = NULL) {
  ee = parse(text = sprintf("%s$new(env = env, conf = conf)", name))
  agent = eval(ee)  # the text is with respect to the passed arguments
  env$setAgent(agent)  # so env has hook to all objects in agent
  agent
}
