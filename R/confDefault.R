# define default hyper-parameters
Conf4DQN = list()
Conf4DQN[["agent"]] = list(
agentname = "DQN",
replayBatchSize = 5L,
GAMMA = 0.99,
EPSILON = 1.0,
fixedEpsilon = 0.01,
decay = exp(-1.0 / 10),
memname = "priorityAbs",
policy = "policy.predProbRank",
calTD = FALSE,
memLaplaceSmoother = 0.001  # avoid divide by zero error
)

Conf4DQN[["gym"]] = list(
  scenarioname = "MountainCar-v0",
  render = TRUE  ## disable this for benchmark
)

Conf4DQN[["interact"]] = list(
  maxiter = 1000L,
  save.flag = TRUE,
  beforeActPipe = c("render", "epi-step-log"),
  afterStepPipe = c("after", "replay")
)

Conf4DQN[["nn"]] = list(
  EPOCH = 1L,   # how many epoch to apply for each training
  archname = "mountaincar-linear-noreg"
)

# The following fields do not affect performance
Conf4DQN[["performance"]] = list(
resultTbPath = "Perf.RData"  #  will be put under ROOTFOLDERNAME
)

Conf4DQN[["logging"]] = list(
ROOTFOLDERNAME = "logout",
LOGGERNAMENN = "nn.logger",
LOGGERNAMERL = "rl.logger",
NNSufix = "nn.log",
RLSufix = "rl.log"
)




# define default hyper-parameters
RLConfDefault = list()
RLConfDefault[["dqn"]] = Conf4DQN

