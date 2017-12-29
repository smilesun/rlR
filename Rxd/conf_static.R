RLConf$static = list()

RLConf$static$agent = list(
agentname = "DQN",
replayBatchSize = 5L,
GAMMA = 0.99,  # Degradation factor
EPSILON = 0.9,
decay = exp(-1/10) # half time is 10 time step
)

RLConf$static$gym = list(
  scenarioname = "MountainCar-v0"
)

RLConf$static$interact = list(
  maxiter = 50L
)

RLConf$static$nn = list (
  EPOCH = 1L,  # FOR DEBUG
  archname = "mountaincar-linear-noreg")

# The following fields do not affect performance so is hard coded
RLConf$static[["performance"]] = list(
resultTbPath = "../output/Perf.RData"
)

RLConf$static[["logging"]] = list(
ROOTFOLDERNAME = "../log",
LOGGERNAMENN = 'nn.logger',
LOGGERNAMERL = 'rl.logger',
NNSufix = 'nn.log',
RLSufix = 'rl.log'
)

