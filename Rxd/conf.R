# configuration namespace seperate the configuration parameter for different use
RLConf = R6Class("Conf",
  public = list(
  dict = list(),   
  # dollar.path is the series of namespace  nn$firstLayer$lastLayer
  update = function(namespace, dollar.path) {

  }
  ),
  private = list(),
  active = list()
  )

RLConf$static = list(
inst.perScenario = 20L,
time.step = 100L,
replayBatchSize = 5L,
GAMMA = 0.99,  # Degradation factor
# BATCH_SIZE = 5,
EPOCH = 1L,  # FOR DEBUG
VAL_RATE = 0.1,
EPSILON = 0.05,
resultTbPath = "../output/Perf.RData",
LOGGERNAME = 'surrogate.nn',
LOGGERFILENN = '../log/nn.log',
LOGGERFILEROOT = '../log/rf.log',
firstLayer = list(unit = 64, activation ="relu"),
secondLayer = list(unit = 64, activation ="relu")
)

RLConf$test = function() {
  conf = RLConf$new()
  conf$update("surrogate", list("firstLayer" = firstLayer, "secondLayer" = secondLayer))
}
