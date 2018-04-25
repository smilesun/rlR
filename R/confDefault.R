# define default hyper-parameters
rlR.conf.default = list(
agent.name = "DQN",
agent.archname = "mountaincar",
agent.gamma = 0.99,
policy.epsilon = 0.01,
policy.decay = exp(-1.0 / 10),
policy.name = "epsilonGreedy",
replay.memname = "latest",
replay.batchsize = 5L,
replay.mem.laplace.smoother = 0.001,
replay.epoch = 1L,
interact.scenarioname = "MountainCar-v0",
interact.render = TRUE,
interact.maxiter = 1000L,
interact.beforeActPipe = c("render", "epi-step-log"),
interact.afterStepPipe = c("after", "replay")
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
