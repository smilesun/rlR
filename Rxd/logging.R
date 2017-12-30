logReset()
RLLog = R6Class("RLLog",
  public = list(
    log.root = NULL,
    log.nn = NULL,
    cmdPattern.Dict =  list("root"="log.root"),
    log = function(type, msg) {
      self$log.root
    },
    initialize = function(RLConf = RLConf) {
      conf.logging = RLConf$fetchConf("logging") 
      self$log.root = getLogger(conf.logging$LOGGERNAMERL)
      self$log.nn = getLogger(conf.logging$LOGGERNAMENN)
      str.conf = toString(RLConf$static)
      hash.conf = md5(str.conf)
      str.time = toString(Sys.time())
      str.time = gsub(" ","_",str.time)
      str.date = toString(Sys.Date())
      filePrefix = file.path(getwd(), conf.logging$ROOTFOLDERNAME, str.date, hash.conf, str.time)
      cat(sprintf("logging file path %s", filePrefix))
      dir.create(filePrefix, recursive = TRUE) # rl.log and nn.log are under this directory
      addHandler(writeToFile, file = file.path(filePrefix, conf.logging$RLSufix), logger = conf.logging$LOGGERNAMERL) # default logger is the root handler
      removeHandler("writeToConsole", logger = conf.logging$LOGGERNAMENN)
      removeHandler("basic.stdout", logger = conf.logging$LOGGERNAMENN)
      addHandler(writeToFile, file = file.path(filePrefix,conf.logging$NNSufix), logger = conf.logging$LOGGERNAMENN)
    },
    logConf = function() {
      str.conf = toString(RLConf$static)
      self$log.root$info(str.conf)
    }
    )
)

