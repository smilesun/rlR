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
agent.update.target.freq = NULL,
agent.start.learn = NULL,  # default after replay mem has enough sample
agent.clip.td = FALSE,
policy.maxEpsilon = 0.01,
policy.minEpsilon = 0.01,
policy.decay = 1, # exp(-1.0 / 10),
policy.decay.type = "geometric1",
policy.aneal.steps = 1e6,
policy.softmax.magnify = 1,
replay.batchsize = 64,
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
    return(list.conf[[agent_name]])
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
