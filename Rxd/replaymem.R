ReplayMem = R6Class("ReplayMem",
  public = list(
    samples = NULL,
    dt = NULL,
    len = 0,
    list.sample.fun = list(),
    priority = NULL,
    replayed.idx = NULL,
    initialize = function(name) {
      self$samples = list()
      self$dt = data.table()
      self$len = 0
    },

    add = function(ins) {  # ins = ReplayMem$mkInst(...)
      len = length(self$samples)
      self$samples[[len + 1]] = ins
      self$len = self$len + 1
      mcolnames = names(unlist(ins))
      mdt = data.table(t(unlist(ins)))
      mdt[,c("priorityAbs", "priorityRank")] = c(NA,NA)
      self$dt = rbindlist(list(self$dt, mdt))
      self$updatePriority()
    },

    updatePriority = function() {
      self$dt$priorityAbs = (self$dt[,delta] + RLConf$static$agent$memLaplaceSmoother)
      self$dt$priorityRank = 1/order(self$dt$delta, decreasing = TRUE)
    },

    ins.sample.earlierst = function(k) {
      k = min(k, self$len)
      list.res = lapply(sample(self$len)[1:k], function(x) self$samples[[x]])
      return(list.res)
    },

    ins.sample.latest = function(k) {
      k = min(k, self$len)
      x = (self$len - k + 1L): self$len
      list.res = lapply(x, function(x) self$samples[[x]])
      return(list.res)
    },

    ins.sample.all = function(k = self$len) {
      self$ins.sample.latest(self$len)
    }

    ),
  private = list(),
  active = list()
  )

ReplayMem$factory = function(name) {
  hash = list(
    "uniform" = ReplayMemUniform, 
    "latest" = ReplayMemLatest,
    "priorityAbs" = ReplayMemPrioritizedAbs,
    "priorityRank" = ReplayMemPrioritizedRank
    )
  return(hash[[name]]$new)
}

ReplayMem$mkInst = function(state.old, action, reward, state.new, delta) { 
  if(is.null(delta)) delta = NA
  list(state.old = state.old, action = action, reward = reward, state.new = state.new, delta = delta) }

ReplayMem$extractOldState = function(x) {
      return(x[[1L]])
    }

ReplayMem$extractNextState = function(x) {
      return(x[[4L]])
    }

ReplayMem$extractReward = function(x) {
      return(x[[3L]])
    }


ReplayMemUniform = R6Class("ReplayMemUniform",
  inherit = ReplayMem,
  public = list(
    initialize = function(name ="uniform-all") {
      super$initialize(name)
    },
    sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample(self$len)[1:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    }
    ),
  private = list(),
  active = list()
  )


ReplayMemLatest = R6Class("ReplayMemLatest",
  inherit = ReplayMem,
  public = list(
    initialize = function(name ="uniform-all") {
      super$initialize(name)
    },
   sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = (self$len - k + 1L): self$len
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    }
    ),
  private = list(),
  active = list()
  )


ReplayMemPrioritizedAbs = R6Class("ReplayMemPrioritizedAbs",
  inherit = ReplayMem,
  public = list(
    initialize = function(name ="priority") {
      super$initialize(name)
    },
    sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample.int(self$len, prob = self$dt$priorityAbs)[1:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    }
    ),
  private = list(),
  active = list()
  )

ReplayMemPrioritizedRank = R6Class("ReplayMemPrioritizedRank",
  inherit = ReplayMem,
  public = list(
    initialize = function(name ="priority") {
      super$initialize(name)
    },
    sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample.int(self$len, prob = self$dt$priorityRank)[1:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    }
    ),
  private = list(),
  active = list()
  )

