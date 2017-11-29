logReset()
log.root = getLogger("")
log.nn = getLogger(RLConf$static$LOGGERNAME)
unlink(RLConf$static$LOGGERFILEROOT)
unlink(RLConf$static$LOGGERFILE)
addHandler(writeToFile, file = RLConf$static$LOGGERFILEROOT, level = 'DEBUG', logger = "") # default logger is the root handler
addHandler(writeToFile, file = RLConf$static$LOGGERFILE, level = 'DEBUG', logger = RLConf$static$LOGGERNAME)
# log.nn$info("surrogate.nn logger initialized")
# with(getLogger(), names(handlers))
# ls(getLogger())
#' getLogger()[['level']]
# getLogger()[['handlers']]
# loginfo('does it work?')
# logwarn('my %s is %d', 'name', 5)
# logdebug('I am silent child') # anything that has a lower priority than the handler will not be logged
# getHandler('basic.stdout')[['level']]
# setLevel(0, getHandler('basic.stdout'))
