ReplayMem = R6Class("ReplayMem",
  public = list(
    samples = NULL,
    dt = NULL,
    len = NULL,
    replayed.idx = NULL,
    conf = NULL,
    agent = NULL,
    initialize = function(agent, conf) {
      self$samples = list()
      self$dt = data.table()
      self$len = 0L
      self$conf = conf
      self$agent = agent
    },

    mkInst = function(state.old, action, reward, state.new, delta = NULL, context = NULL) {
      if (is.null(delta)) delta = NA
      ins = list(state.old = state.old, action = action, reward = reward, state.new = state.new, delta = delta)
      # delta = self$agent$calculateTDError(ins)
      ins$delta = delta
      ins$deltaOfdelta = NA
      ins$deltaOfdeltaPercentage = NA
      ins$context = context  # for extension
      ins
    },

    add = function(ins) {
      len = length(self$samples)
      self$samples[[len + 1L]] = ins
      self$len = self$len + 1L
      mcolnames = names(unlist(ins))
      mdt = data.table(t(unlist(ins)))
      dt.temp = data.table("priorityAbs" = NA, "priorityRank" = NA, "priorityDelta2" = NA)  # FIXME: is there a way to predefine this somewhere else instead of hard coded here?
      mdt = cbind(mdt, dt.temp)
      self$dt = rbindlist(list(self$dt, mdt))
      self$updatePriority()
    },

    updateDT = function(yhat, y) {
      err = yhat - y
      updatedTDError = rowSums(err ^ 2)
      old.delta = self$dt[self$mem$replayed.idx, "delta"]
      self$dt[self$replayed.idx, "delta"] = updatedTDError
      self$dt[self$replayed.idx, "deltaOfdelta"] = updatedTDError - old.delta
      self$dt[self$replayed.idx, "deltaOfdeltaPercentage"] = abs(self$dt[self$replayed.idx, "deltaOfdelta"]) / abs(old.delta)
      self$updatePriority()
  },

    updatePriority = function() {
      self$dt[, "priorityAbs"] = (abs(self$dt[, "delta"]) + self$conf$get("replay.mem.laplace.smoother"))
      self$dt[, "priorityRank"] = order(self$dt[, "delta"])
      self$dt[, "priorityDelta2"] = abs(self$dt[, "deltaOfdelta"])
    }
    ),
  private = list(),
  active = list()
  )

ReplayMem$ins2String = function(x) x

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

ReplayMemUniform = R6Class("ReplayMemUniform",
  inherit = ReplayMem,
  public = list(
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
    sample.fun = function(k) {
      k = min(k, self$len)
      if (any(is.na(self$dt$priorityAbs))) {
        self$replayed.idx = sample.int(self$len)
      } else {
      self$replayed.idx = sample.int(self$len, prob = self$dt$priorityAbs)[1L:k]   # FIXME: programe stoppped execution once self$dt$priorityAbs has NA
      }
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
  all = getNamespaceExports("rlR")
  mem.idx = which(sapply(all, function(x) grepl("ReplayMem", x)))
  assert(paste0("ReplayMem", name) %in% all[mem.idx])
  return(eval(parse(text = sprintf("ReplayMem%s$new", name))))
}
