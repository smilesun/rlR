# define default hyper-parameters
RLConf$static = list()



RLConf$static[["agent"]] = list(
agentname = "DQN",
replayBatchSize = 5L,
GAMMA = 0.99,  # Degradation factor
EPSILON = 1.0,
decay = exp(-1/10) # half time is 10 time step to 0.3678, after 30 steps, it is 0.049787
)

RLConf$static[["gym"]] = list(
  scenarioname = "MountainCar-v0"
)

RLConf$static[["interact"]] = list(
  maxiter = 50L,
  save.flag = TRUE
)

RLConf$static[["nn"]] = list (
  EPOCH = 1L,  
  archname = "mountaincar-linear-noreg"
)

# The following fields do not affect performance
RLConf$static[["performance"]] = list(
resultTbPath = "Perf.RData"  #  will be put under ROOTFOLDERNAME
)

RLConf$static[["logging"]] = list(
ROOTFOLDERNAME = "../logout",
LOGGERNAMENN = 'nn.logger',
LOGGERNAMERL = 'rl.logger',
NNSufix = 'nn.log',
RLSufix = 'rl.log'
)

