logReset()
RLLog = R6Class("RLLog",
  public = list(
    log.root = NULL,
    log.nn = NULL,
    conf = NULL, 
    # the configuration of logging does not impact the performance, so use global configuration
    initialize = function(conf) {   
      conf.logging = conf$fetchConf("logging") 
      self$log.root = getLogger(conf.logging$LOGGERNAMERL)
      self$log.nn = getLogger(conf.logging$LOGGERNAMENN)
      str.conf = toString(conf$static)  # experiment specific configuration
      hash.conf = md5(str.conf)
      str.time = toString(Sys.time())
      str.time = gsub(" ","_",str.time)
      str.date = toString(Sys.Date())
      filePrefix = file.path(getwd(), conf.logging$ROOTFOLDERNAME, str.date, hash.conf, str.time)
      cat(sprintf("logout file path %s", filePrefix))
      conf$static$performance$filePrefix = filePrefix
      conf$static$performance$resultTbPath =  file.path(filePrefix, conf$static$performance$resultTbPath)  # RData file persistence place
      dir.create(filePrefix, recursive = TRUE)
      addHandler(writeToFile, file = file.path(filePrefix, conf.logging$RLSufix), logger = conf.logging$LOGGERNAMERL)
      removeHandler("writeToConsole", logger = conf.logging$LOGGERNAMENN)
      removeHandler("basic.stdout", logger = conf.logging$LOGGERNAMENN)
      addHandler(writeToFile, file = file.path(filePrefix,conf.logging$NNSufix), logger = conf.logging$LOGGERNAMENN)
      self$log.root$info(str.conf)
      self$log.root$info(filePrefix)  # take down the directory name
      info = paste0("\n", conf$static[["performance"]]$info.before, conf$static$performance$filePrefix, conf$static[["performance"]]$info.after)
      self$log.root$info(info)
    },
    toConsole = function(...) {
    }
    )
)

