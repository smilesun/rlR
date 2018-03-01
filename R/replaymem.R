ReplayMem = R6Class("ReplayMem",
  public = list(
    samples = NULL,
    dt = NULL,
    len = NULL,
    replayed.idx = NULL,
    conf = NULL,
    initialize = function(conf) {
      self$samples = list()
      self$dt = data.table()
      self$len = 0L
      self$conf = conf
    },

    # ins = ReplayMem$mkInst(...)
    add = function(ins) {        
      len = length(self$samples)
      self$samples[[len + 1L]] = ins
      self$len = self$len + 1L
      mcolnames = names(unlist(ins))
      mdt = data.table(t(unlist(ins)))
      dt.temp = data.table("priorityAbs" = NA, "priorityRank" = NA, "priorityDelta2" = NA)  # FIXME: is there a way to predefine this somewhere else instead of hard coded here?
      mdt = cbind(mdt,dt.temp)
      self$dt = rbindlist(list(self$dt, mdt))
      self$updatePriority()
    },

    
    updatePriority = function() {
      self$dt[, "priorityAbs"] = (abs(self$dt[,"delta"]) + self$conf$static$agent$memLaplaceSmoother)
      self$dt[, "priorityRank"] = order(self$dt[,"delta"])
      self$dt[, "priorityDelta2"] = abs(self$dt[,"deltaOfdelta"])
    }
    ),
  private = list(),
  active = list()
  )

ReplayMem$inst2string = function() {

}
ReplayMem$mkInst = function(state.old, action, reward, state.new, delta = NULL) { 
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
    initialize = function(conf) {
      super$initialize(conf)
    },
    sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample(self$len)[1L:k]
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
    initialize = function(conf) {
      super$initialize(conf)
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

ReplayMemLatestProb = R6Class("ReplayMemLatestProb",
  inherit = ReplayMem,
  public = list(
    initialize = function(conf) {
      super$initialize(conf)
    },
   sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample(self$len, prob = 1L:self$len, size = k)
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
    initialize = function(conf) {
      super$initialize(conf)
    },
    sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample.int(self$len, prob = self$dt$priorityAbs)[1L:k]   # FIXME: programe stoppped execution once self$dt$priorityAbs has NA
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
    initialize = function(conf) {
      super$initialize(conf)
    },
    sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample.int(self$len, prob = self$dt$priorityRank)[1L:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
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
    "priorityRank" = ReplayMemPrioritizedRank,
    "latestprob" =  ReplayMemLatestProb
    )
  return(hash[[name]]$new)
}
