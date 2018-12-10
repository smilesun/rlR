#' @title Initialize an Agent with an environment
#' @description Initialize the agent with an environment from where the agent can learn.
#' @param name The name of the Agent
#' @param env The environment to initialize the Agent
#' @param conf The configuration
#' @return [\code{\link{AgentArmed}}].
#' @examples initAgent("AgentDQN", makeGymEnv(name ="CartPole-v0"), getDefaultConf("AgentDQN"))
#' @export
initAgent = function(name, env, conf = NULL) {
  ee = parse(text = sprintf("%s$new(env = env, conf = conf)", name))
  agent = eval(ee)  # the text is with respect to the passed arguments
  env$setAgent(agent)  # so env has hook to all objects in agent
  agent
}
