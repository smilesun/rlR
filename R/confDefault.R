# define default hyper-parameters
rlR.conf.default = list(
agent.name = "DQN",
agent.archname = "dqn",
agent.gamma = 0.99,
agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.0025),
policy.epsilon = 0.01,
policy.decay = 1, # exp(-1.0 / 10),
policy.name = "policy.epsilonGreedy",
replay.memname = "uniform",
replay.batchsize = 25L,
replay.mem.laplace.smoother = 0.001,
replay.epoch = 1L,
interact.maxiter = 500L,
interact.beforeActPipe = c("render", "epi-step-log"),
interact.afterStepPipe = c("after.step", "replay")
)

rlR.conf.avail = names(rlR.conf.default)

rlR.conf4log = list(
resultTbPath = "Perf.RData",  #  will be put under ROOTFOLDERNAME
ROOTFOLDERNAME = "logout",
LOGGERNAMENN = "nn.logger",
LOGGERNAMERL = "rl.logger",
NNSufix = "nn.log",
RLSufix = "rl.log"
)
