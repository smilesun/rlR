RLConf$static = list(
inst.perScenario = 20L,
time.step = 100L,
replayBatchSize = 5L,
GAMMA = 0.99,  # Degradation factor
EPOCH = 1L,  # FOR DEBUG
VAL_RATE = 0.1,
EPSILON = 0.05,
)

# The following fields do not affect performance so is hard coded
RLConf$static[["performance"]] = list(
resultTbPath = "../output/Perf.RData"
)
RLConf$static[["logging"]] = list(
LOGGERNAME = 'surrogate.nn',
LOGGERFILENN = '../log/nn.log',
LOGGERFILEROOT = '../log/rf.log'
)

RLConf$test = function() {
  conf = RLConf$new()
  conf$update("surrogate", list("firstLayer" = firstLayer, "secondLayer" = secondLayer))
}
