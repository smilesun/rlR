logReset()
conf.logging = RLConf$fetchConf("logging") 
unlink(conf.logging$LOGGERFILEROOT) #  delete the old log
unlink(conf.logging$LOGGERFILENN)  # delete the old log
dir.create(dirname(conf.logging$LOGGERFILEROOT))
dir.create(dirname(conf.logging$LOGGERFILENN))
log.root = getLogger("root.logger")
addHandler(writeToFile, file = conf.logging$LOGGERFILEROOT, logger = "root.logger") # default logger is the root handler
#
log.nn = getLogger(conf.logging$LOGGERNAME)
removeHandler("writeToConsole", logger = conf.logging$LOGGERNAME)
removeHandler("basic.stdout", logger = conf.logging$LOGGERNAME)
addHandler(writeToFile, file = conf.logging$LOGGERFILENN, logger = conf.logging$LOGGERNAME)
