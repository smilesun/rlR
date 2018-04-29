logReset()
RLLog = R6Class("RLLog",
  public = list(
    log.root = NULL,
    log.nn = NULL,
    conf = NULL,
    # the configuration of logging does not impact the performance, so use global configuration
    initialize = function(conf) {
      conf.logging = conf$conf.log.perf
      self$log.root = getLogger(conf$conf.log.perf$LOGGERNAMERL)
      self$log.nn = getLogger(conf$conf.log.perf$LOGGERNAMENN)
      dir.create(conf$conf.log.perf$filePrefix, recursive = TRUE)
      addHandler(writeToFile, file = file.path(conf$conf.log.perf$filePrefix, conf.logging$RLSufix), logger = conf.logging$LOGGERNAMERL)
      removeHandler("writeToConsole", logger = conf$conf.log.perf$LOGGERNAMENN)
      removeHandler("basic.stdout", logger = conf$conf.log.perf$LOGGERNAMENN)
      addHandler(writeToFile, file = file.path(conf.logging$filePrefix, conf$conf.log.perf$NNSufix), logger = conf$conf.log.perf$LOGGERNAMENN)
      self$log.root$info(conf.logging$str.conf)
      self$log.root$info(conf$conf.log.perf$filePrefix)  # take down the directory name
      info = paste0("\n", conf.logging$info.before, conf.logging$filePrefix, conf.logging$info.after)
      self$log.root$info(info)
    },
    toConsole = function(...) {
    }
    )
)
