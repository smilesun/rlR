# define default hyper-parameters
RLConfDefault = list()

RLConfDefault[["agent"]] = list(
agentname = "DQN",
replayBatchSize = 5L,   # for uniform sampling
GAMMA = 0.99,  # Degradation factor
EPSILON = 1.0,
fixedEpsilon = 0.01,  # one time configuration, without being varied
decay = exp(-1.0/10), # half time is 10 time step to 0.3678, after 30 steps, it is 0.049787
memname = "priorityAbs",
policy = "policy.predProbRank",
calTD = FALSE,
memLaplaceSmoother = 0.001  # avoid divide by zero error
)

RLConfDefault[["gym"]] = list(
  scenarioname = "MountainCar-v0",
  render = TRUE  ## disable this for benchmark
)

RLConfDefault[["interact"]] = list(
  maxiter = 1000L,
  save.flag = TRUE
)

RLConfDefault[["nn"]] = list (
  EPOCH = 1L,   # how many epoch to apply for each training
  archname = "mountaincar-linear-noreg"
)

# The following fields do not affect performance
RLConfDefault[["performance"]] = list(
resultTbPath = "Perf.RData"  #  will be put under ROOTFOLDERNAME
)

RLConfDefault[["logging"]] = list(
ROOTFOLDERNAME = "../logout",
LOGGERNAMENN = 'nn.logger',
LOGGERNAMERL = 'rl.logger',
NNSufix = 'nn.log',
RLSufix = 'rl.log'
)

