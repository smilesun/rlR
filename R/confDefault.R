# define default hyper-parameters
rlR.conf.default = list(
render = TRUE,
agent.gamma = 0.99,
brain.build_fun = NULL,  # user specific function to create surrogate model
policy.maxEpsilon = 0.01,
policy.minEpsilon = 0.01,
policy.decay = 1, # exp(-1.0 / 10),
replay.memname = "Uniform",
replay.epochs = 1L,
interact.maxiter = 500L,
policy.epi_wait_ini = 100L,
policy.epi_wait_expl = 20L
)


rlR.conf.avail = names(rlR.conf.default)

rlR.conf4log = list(
replay.mem.laplace.smoother = 0.001,
resultTbPath = "Perf.RData",
ROOTFOLDERNAME = "logout",
LOGGERNAMENN = "nn.logger",
LOGGERNAMERL = "rl.logger",
NNSufix = "nn.log",
RLSufix = "rl.log.R"
)
