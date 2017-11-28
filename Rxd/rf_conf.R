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

replayBatchSize = 5L,
GAMMA = 0.99,  # Degradation factor
# BATCH_SIZE = 5,
EPOCH = 1L,  # FOR DEBUG
VAL_RATE = 0.1,
EPSILON = 0.05,
LOGGERNAME = 'surrogate.nn',
LOGGERFILE = '../nn.log',
LOGGERFILEROOT = '../rf.log',
firstLayer = list(unit = 64, activation ="relu"),
secondLayer = list(unit = 64, activation ="relu")
)

RLConf$test = function() {
  conf = RLConf$new()
  conf$update("surrogate", list("firstLayer" = firstLayer, "secondLayer" = secondLayer))
}
