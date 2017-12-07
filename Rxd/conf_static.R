RLConf$static = list()

RLConf$static$agent = list(
replayBatchSize = 5L,
GAMMA = 0.99,  # Degradation factor
EPSILON = 0.05
)

RLConf$static$nn = list (
  EPOCH = 1L,  # FOR DEBUG
  archname = "mountaincar-linear-noreg")

# The following fields do not affect performance so is hard coded
RLConf$static[["performance"]] = list(
resultTbPath = "../output/Perf.RData"
)
RLConf$static[["logging"]] = list(
LOGGERNAME = 'surrogate.nn',
LOGGERFILENN = '../log/nn.log',
LOGGERFILEROOT = '../log/rf.log'
)

