# define default hyper-parameters
rlR.conf.default = list(
render = FALSE,
log = FALSE,
console = FALSE,
agent.gamma = 0.99,
agent.flag.reset.net = TRUE,
agent.lr.decay = exp(-0.001),
agent.lr = 1e-3,
agent.observ_stack_len = 1L,
agent.network.build.funs = NULL,  # user specific function to create surrogate model
agent.store.model = FALSE,
policy.maxEpsilon = 0.01,
policy.minEpsilon = 0.01,
policy.decay = 1, # exp(-1.0 / 10),
policy.softmax.magnify = 1,
replay.batchsize = 64,
replay.memname = "Uniform",
replay.mem.size = 20000,
replay.epochs = 1L,
replay.freq = 1L
)

rlR.conf.avail = names(rlR.conf.default)

#' @title getDefaultConf
#' @description List defaults hyper-parameters
#' @export
getDefaultConf = function() {
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
