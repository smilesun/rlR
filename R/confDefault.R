# hyper-parameters range
rlR.conf.lod = list(
render = list(name = "render", note = "Whether to show rendering video or not", value = FALSE),
log = list(name = "log", note = "Whether to log important information on drive", value = FALSE),
console = list(name = "console", note = "Whether to enable debug info output to console", value = FALSE),
agent.gamma = list(name = "agent.gamma", note = "The discount factor in reinforcement learning", value = 0.99),
agent.flag.reset.net = list(name = "agent.flag.reset.net", note = "Whether to reset the neural network ", value = TRUE),  #FIXME: should be set this?
agent.lr.decay = list(name = "agent.lr.decay", note = "The decay factor of the learning rate at each step", value = exp(-0.001)),  # decaying with regard to step is better since some episode can be too long 
agent.lr = list(name = "agent.lr", note = "learning rate for the agent", value = 1e-3),
agent.store.model = list(name = "agent.store.model", note = "whether to store the model of the agent or not", value = FALSE),  #FIXME: exclude this
agent.update.target.freq = list(name = "agent.update.target.freq", note = "How often should the target network be set", value = 200L),
agent.start.learn = list(name = "agent.start.learn", note = "after how many transitions should replay begin", value = 64L),
agent.clip.td = list(name = "agent.clip.td", note = "whether to clip TD error", value = FALSE),
policy.maxEpsilon = list(name = "policy.maxEpsilon", note = "The maximum epsilon exploration rate", value = 1.0),
policy.minEpsilon = list(name = "policy.minEpsilon", note = "The minimum epsilon exploration rate", value = 0.01),
policy.decay.rate = list(name = "policy.decay.rate", note = "the decay rate", value = 1.0),
policy.decay.type = list(name = "policy.decay.type", note = "the way to decay epsion, can be decay_geo, decay_exp, decay_linear", value = "decay_geo"),
policy.aneal.steps = list(name = "policy.aneal.steps", note = "only valid when policy.decay.type = 'decay_linear'", value = 1e6),
policy.softmax.magnify = list(name = "policy.softmax.magnify", value = 1),
replay.batchsize = list(name = "replay.batchsize", note = "how many samples to take from replay memory each time", value = 64),
replay.memname = list(name = "replay.memname", range = c("Uniform"), note = "The type of replay memory", value = "Uniform"),
replay.mem.size = list(name = "replay.mem.size", note = "The size of the replay memory", value = 2e4),
replay.epochs = list(name = "replay.epochs", note = "How many gradient decent epochs to carry out for one replay", value = 1L),
replay.freq = list(name = "replay.freq", note = "how many steps to wait until one replay", value = 1L)
)

rlR.conf.dt = data.table::rbindlist(rlR.conf.lod, fill = TRUE)
rlR.conf.df = as.data.frame(rlR.conf.dt)


# define default hyper-parameters
rlR.conf.default = lapply(rlR.conf.lod, function(x) x$value)

#' @title listAvailConf
#' @description List defaults hyper-parameters names
#' @export
listAvailConf = function() {
  rlR.conf.dt
}

# default configuration for each agent which is adjacent to the definition so once definition is modified, it is easy to modify here as well.
rlR.conf.AgentTable = function() {
  RLConf$new(
          render = F,
          console = T,
          log = FALSE,
          agent.lr = 0.5,
          agent.gamma = 0.95,
          policy.maxEpsilon = 0.1,
          policy.minEpsilon = 0,
          policy.decay.type = "decay_linear",
          policy.aneal.steps = 400,
          #policy.decay.rate = exp(-0.001),
          policy.name = "EpsilonGreedy",
          agent.start.learn = 0L)
}

rlR.conf.AgentDQN  = rlR.conf.AgentFDQN  = rlR.conf.AgentDDQN = function() {
  RLConf$new(
          render = FALSE,
          console = TRUE,
          log = FALSE,
          policy.maxEpsilon = 1,
          policy.minEpsilon = 0.01,
          policy.decay.rate = exp(-0.001),
          policy.name = "EpsilonGreedy",
          replay.batchsize = 64L)
}

rlR.conf.AgentPG = rlR.conf.AgentPGBaseline = function() {
  RLConf$new(
          agent.lr = 1e-2,
          render = FALSE,
          console = TRUE,
          flag_rescue = FALSE,
          agent.gamma = 0.99,
          policy.maxEpsilon = 0,
          policy.minEpsilon = 0,
          agent.flag.reset.net = FALSE,
          policy.softmax.base = 1,
          policy.softmax.magnify = 1.001,
          policy.name = "Prob",
          replay.memname = "Latest",
          replay.epochs = 1L)
}

rlR.conf.AgentActorCritic = function() {
  conf = RLConf$new(
    render = FALSE,
    log = FALSE,
    agent.lr = 1e-2,
    agent.gamma = 0.9,
    agent.lr.decay = 1,
    console = TRUE,
    policy.name = "Prob",
    policy.maxEpsilon = 0,
    policy.minEpsilon = 0,
    replay.epochs = 1L,
    replay.memname = "Latest"
    #agent.nn.arch.actor = list(nhidden = 64, act1 = "tanh", act2 = "softmax", loss = "categorical_crossentropy", lr = 1e-4, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=1e-4)", decay = 0.9, clipnorm = 5),
    #agent.nn.arch.critic = list(nhidden = 64, act1 = "tanh", act2 = "linear", loss = "mse", lr =1e-4, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=1e-4)", decay = 0.9, clipnorm = 5)
    )
}

rlR.conf.AgentDDPG = function() {
  conf = RLConf$new(
    render = FALSE,
    log = FALSE,
    agent.lr = 1e-2,
    agent.gamma = 0.9,
    agent.lr.decay = 1,
    console = TRUE,
    policy.name = "Prob",
    policy.maxEpsilon = 0,
    policy.minEpsilon = 0,
    replay.epochs = 1L,
    replay.memname = "Uniform"
    )
}


#' @title get Default Configuration according to agent name
#' @description List defaults hyper-parameters
#' @param agent_name The name for Agent
#' @export
#' @examples
#' conf = rlR::getDefaultConf("AgentDQN")
getDefaultConf = function(agent_name) {
    get(paste0("rlR.conf.", agent_name))()
}

#' @title show Default Configuration
#' @description List defaults hyper-parameters in dataframe
#' @export
#' @examples
#' df = rlR::showDefaultConf()
showDefaultConf = function() {
  rlR.conf.df = data.frame(unlist(rlR.conf.default))
  colnames(rlR.conf.df) = NULL
  rlR.conf.df
}


rlR.conf4log = list(
policy.epi_wait_ini = 5L,  # initially the performance should increase
policy.epi_wait_middle = 25L,
policy.epi_wait_expl = 40L,
replay.mem.dt = FALSE,
replay.mem.laplace.smoother = 0.001,
resultTbPath = "Perf.RData",
LOGGERNAMENN = "nn.logger",
LOGGERNAMERL = "rl.logger",
NNSufix = "nn.log",
RLSufix = "rl.log.R"
)


agent.brain.dict.AgentDQN =  agent.brain.dict.AgentFDQN = agent.brain.dict.AgentDDQN = function() list(value_fun = rlR:::makeValueNet)
agent.brain.dict.AgentPG = function() list(policy_fun = rlR:::makePolicyNet)
agent.brain.dict.AgentPGBaseline  = function() list(policy_fun = rlR:::makePolicyNet, value_fun = rlR:::makeValueNet)
agent.brain.dict.AgentActorCritic = function() list(policy_fun = rlR:::makePolicyNet2, value_fun = rlR:::makeValueNet2)
