ReplayMem$extractOldState = function(x) {
  return(x[[1L]])
}

ReplayMem$extractAction = function(x) {
  return(x[[2L]])
}

ReplayMem$extractReward = function(x) {
  return(x[[3L]])
}

ReplayMem$extractNextState = function(x) {
  return(x[[4L]])
}
ReplayMem$extractDone = function(x) {
  return(x[[5L]])
}
ReplayMem$extractStep = function(x) {
  return(x[[6L]][["stepidx"]])
}
