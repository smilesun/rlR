logReset()
# basicConfig()
unlink(RLConf$static$LOGGERFILEROOT) #  delete the old log
unlink(RLConf$static$LOGGERFILENN)  # delete the old log
log.root = getLogger("root.logger")
addHandler(writeToFile, file = RLConf$static$LOGGERFILEROOT, logger = "root.logger") # default logger is the root handler
#
log.nn = getLogger(RLConf$static$LOGGERNAME)
removeHandler("writeToConsole", logger = RLConf$static$LOGGERNAME)
removeHandler("basic.stdout", logger = RLConf$static$LOGGERNAME)
addHandler(writeToFile, file = RLConf$static$LOGGERFILENN, logger = RLConf$static$LOGGERNAME)
