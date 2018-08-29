# hyper-parameters range
rlR.conf.lod = list(
render = list(note = "Whether to show rendering video or not"),
log = list(note = "Whether to log important information on drive"),
console = list(note = "Whether to enable debug info output to console"),
agent.gamma = list(note = "The discount factor in reinforcement learning"),
agent.flag.reset.net = list(note = "Whether to reset the neural network "),  #FIXME: should be set this?
agent.lr.decay = list(note = "The decay factor of the learning rate at each step"),  # decaying with regard to step is better since some episode can be too long 
agent.lr = list(note = "learning rate for the agent"),
agent.store.model = list(note = "whether to store the model of the agent or not"),  #FIXME: exclude this
agent.update.target.freq = list(note = "How often should the target network be set"),
agent.start.learn = list(note = "after how many transitions should replay begin"),
agent.clip.td = list(note = "whether to clip TD error"),
policy.maxEpsilon = list(note = "The maximum epsilon exploration rate"),
policy.minEpsilon = list(note = "The minimum epsilon exploration rate"),
policy.decay.rate = list(note = "the decay rate"),
policy.decay.type = list(note = "the way to decay epsion, can be decay_geo, decay_exp, decay_linear"),
policy.aneal.steps = list(note = "how many steps needed to decay from maximum epsilon to minmum epsilon, only valid when policy.decay.type = 'decay_linear'"),
policy.softmax.magnify = 1,
replay.batchsize = 64,
replay.memname = list(range = c("Uniform"), note = "The type of replay memory"),
replay.mem.size = list(range = "integer", note = "The size of the replay memory"),
replay.epochs = list(range = "integer", note = "How many gradient decent epochs to carry out for one replay"),
replay.freq = list(note = "how many steps to wait until one replay")
)

# define default hyper-parameters
rlR.conf.default = list(
render = FALSE,
log = FALSE,
console = FALSE,
agent.gamma = 0.99,
agent.flag.reset.net = TRUE,
agent.lr.decay = exp(-0.001),
agent.lr = 1e-3,
agent.store.model = FALSE,
agent.update.target.freq = 200L,
agent.start.learn = 64L,  # default: after replay mem has enough sample for replay
agent.clip.td = FALSE,
policy.maxEpsilon = 1,
policy.minEpsilon = 0.01,
policy.decay.rate = 1, # exp(-1.0 / 10),
policy.decay.type = "decay_geo",
policy.aneal.steps = 1e6,
policy.softmax.magnify = 1,
replay.batchsize = 64L,
replay.memname = "Uniform",
replay.mem.size = 20000,
replay.epochs = 1L,
replay.freq = 1L
)

#' @title listAvailConf
#' @description List defaults hyper-parameters names
#' @export
listAvailConf = function() {
  rlR_conf_avail = names(rlR.conf.default)
  return(rlR_conf_avail)
}

# default configuration for each agent which is adjacent to the definition so once definition is modified, it is easy to modify here as well.
rlR.conf.DQN = function() {
  RLConf$new(
          render = FALSE,
          console = FALSE,
          log = FALSE,
          policy.maxEpsilon = 1,
          policy.minEpsilon = 0.01,
          policy.decay.rate = exp(-0.001),
          policy.name = "ProbEpsilon",
          replay.batchsize = 64L,
          agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))
}

rlR.conf.FDQN = function() {
  rlR.conf.DQN()
}

rlR.conf.PG = function() {
  RLConf$new(
          render = FALSE,
          console = FALSE,
          policy.maxEpsilon = 1,
          policy.decay = exp(-0.001),
          policy.minEpsilon = 0.01,
          policy.name = "ProbEpsilon",
          replay.memname = "Latest",
          replay.epochs = 1L,
          agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "softmax", loss = "categorical_crossentropy", lr = 25e-3, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0)"))
}

rlR.conf.AC = function() {
  conf = RLConf$new(
    render = FALSE,
    log = FALSE,
    console = FALSE,
    policy.name = "EpsilonGreedy",
    policy.maxEpsilon = 1,
    policy.minEpsilon = 0.02,
    policy.decay = exp(-0.001),
    replay.epochs = 1L,
    replay.memname = "Latest",
    agent.nn.arch.actor = list(nhidden = 64, act1 = "tanh", act2 = "softmax", loss = "categorical_crossentropy", lr = 1e-4, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=1e-4)", decay = 0.9, clipnorm = 5),
    agent.nn.arch.critic = list(nhidden = 64, act1 = "tanh", act2 = "linear", loss = "mse", lr =1e-4, kernel_regularizer = "regularizer_l2(l=0.0001)", bias_regularizer = "regularizer_l2(l=1e-4)", decay = 0.9, clipnorm = 5)
    )
}


#' @title getDefaultConf
#' @description List defaults hyper-parameters
#' @param agent_name The name for Agent
#' @export
#' @examples
#' conf = rlR::getDefaultConf("AgentDQN")
getDefaultConf = function(agent_name) {
    list.conf = list()
    list.conf[["AgentActorCritic"]] = rlR.conf.AC()
    list.conf[["AgentDQN"]] = rlR.conf.DQN()
    list.conf[["AgentFDQN"]] = rlR.conf.FDQN()
    list.conf[["AgentDDQN"]] = rlR.conf.DDQN()
    list.conf[["AgentPG"]] = rlR.conf.PG()
    list.conf[["AgentPGBaseline"]] = rlR.conf.PGBaseline()
    res = list.conf[[agent_name]]
    if(is.null(res)) stop("no such configuration")
    return(res)
}

#' @title showDefaultConf
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
