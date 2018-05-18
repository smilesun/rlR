# define default hyper-parameters
rlR.conf.default = list(
render = TRUE,
agent.name = "DQN",
agent.archname = "dqn",
agent.gamma = 0.99,
agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.0025,  kernel_regularizer = "regularizer_l2(l=0.01)", bias_regularizer = "regularizer_l2(l=0.1)"),
policy.epsilon = 0.01,
policy.minEpsilon = 0.01,
policy.decay = 1, # exp(-1.0 / 10),
replay.memname = "Uniform",
replay.mem.laplace.smoother = 0.001,
replay.epochs = 1L,
interact.maxiter = 500L
)


rlR.conf.avail = names(rlR.conf.default)

rlR.conf4log = list(
resultTbPath = "Perf.RData",  #  will be put under ROOTFOLDERNAME
ROOTFOLDERNAME = "inst/logout",  # R only allows for inst directory to exist
LOGGERNAMENN = "nn.logger",
LOGGERNAMERL = "rl.logger",
NNSufix = "nn.log",
RLSufix = "rl.log.R"
)
