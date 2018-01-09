logReset()
RLLog = R6Class("RLLog",
  public = list(
    log.root = NULL,
    log.nn = NULL,
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
      cat(sprintf("logout file path %s", filePrefix))
      RLConf$static$performance$resultTbPath =  file.path(filePrefix, RLConf$static$performance$resultTbPath)  # RData file persistence place
      dir.create(filePrefix, recursive = TRUE) # rl.log and nn.log are under this directory
      addHandler(writeToFile, file = file.path(filePrefix, conf.logging$RLSufix), logger = conf.logging$LOGGERNAMERL)
      removeHandler("writeToConsole", logger = conf.logging$LOGGERNAMENN)
      removeHandler("basic.stdout", logger = conf.logging$LOGGERNAMENN)
      addHandler(writeToFile, file = file.path(filePrefix,conf.logging$NNSufix), logger = conf.logging$LOGGERNAMENN)
      self$log.root$info(str.conf)
      self$log.root$info(filePrefix)  # take down the folder
    }
    )
)

