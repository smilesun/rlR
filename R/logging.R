logReset()
RLLog = R6::R6Class("RLLog",
  public = list(
    log.root = NULL,
    log.nn = NULL,
    conf = NULL,
    flag = NULL,
    # the configuration of logging does not impact the performance, so use global configuration
    initialize = function(conf) {
      conf.logging = conf$conf.log.perf
      self$conf = conf
      doNothing = function(msg, handler, ...) {
      }
      self$flag = conf$get("log")
      if (is.null(self$flag)) self$flag = FALSE
      # make log obj
      self$log.root = getLogger(conf$conf.log.perf$LOGGERNAMERL)
      self$log.nn = getLogger(conf$conf.log.perf$LOGGERNAMENN)
      # file directory
      if (self$flag) {
      dir.create(conf$conf.log.perf$filePrefix, recursive = TRUE)
      # root logger
      addHandler(writeToFile, file = file.path(conf$conf.log.perf$filePrefix, conf.logging$RLSufix), logger = conf.logging$LOGGERNAMERL)
      # every step logger
      addHandler(writeToFile, file = file.path(conf.logging$filePrefix, conf$conf.log.perf$NNSufix), logger = conf$conf.log.perf$LOGGERNAMENN)
      }
      removeHandler("writeToConsole", logger = conf$conf.log.perf$LOGGERNAMENN)
      removeHandler("basic.stdout", logger = conf$conf.log.perf$LOGGERNAMENN)
      # first logging
      self$log.root$info(conf.logging$str.conf)
      self$log.root$info(conf$conf.log.perf$filePrefix)  # take down the directory name
      info = paste0("\n", conf.logging$info.before, conf.logging$filePrefix, conf.logging$info.after)
      self$log.root$info(info)
    },

    rescue = function() {
      if (self$flag) {
      filename.replay = file.path(rlR.conf4log$filePrefix, "replay.dt.csv")
      filename.experience = file.path(self$conf$conf.log.perf$filePrefix, "experience.dt.csv")
      self$log.root$info("\n a = BBmisc::load2('%s')\n", self$conf$conf.log.perf$resultTbPath)
      cat(sprintf("\n a = BBmisc::load2('%s') \n", self$conf$conf.log.perf$resultTbPath))
      write.csv(self$rl.agent$mem$dt, file = filename.experience)
      self$log.root$info("\n b = read.csv('%s')", filename.experience)
      }
    },

    loginfo = function() {
    },

    toConsole = function(...) {
    }
  )
)
