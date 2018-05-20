#' @export
ReplayMem = R6Class("ReplayMem",
  public = list(
    samples = NULL,
    dt = NULL,
    len = NULL,
    replayed.idx = NULL,
    conf = NULL,
    agent = NULL,
    dt.temp = NULL,
    smooth = NULL,
    initialize = function(agent, conf) {
      self$smooth = rlR.conf4log[["replay.mem.laplace.smoother"]]
      self$samples = list()
      self$dt = data.table()
      self$len = 0L
      self$conf = conf
      self$agent = agent
      # helper constant variable
      self$dt.temp = data.table("delta" = NA, "priorityRank" = NA, "priorityAbs" = NA, "priorityDelta2" = NA, "deltaOfdelta" = NA, "deltaOfdeltaPercentage" = NA)
      self$dt.temp = self$dt.temp[, lapply(.SD, as.numeric)]
    },

    reset = function() {
      self$samples = list()
      self$dt = data.table()
      self$len = 0L
    },

    mkInst = function(state.old, action, reward, state.new, done, info) {
      list(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = info)
    },

    add = function(ins) {
      len = length(self$samples)
      self$samples[[len + 1L]] = ins
      self$len = self$len + 1L
      mdt = data.table(t(unlist(ins)))
      mdt = cbind(mdt, self$dt.temp)
      self$dt = rbindlist(list(self$dt, mdt), fill = TRUE)
    },

    updateDT = function(idx = NULL) {
      if (is.null(idx)) idx = 1L:self$len
      td.list = lapply(idx, function(i) self$agent$calculateTDError(self$samples[[i]]))
      updatedTDError = unlist(td.list)
      cat(sprintf("mean TD error: %f\n", mean(updatedTDError)))
      old.delta = self$dt[idx, "delta"]
      self$dt[idx, "delta"] = updatedTDError
      self$updatePriority()
    },

    afterEpisode = function(interact) {
      # do nothing
    },

    afterStep = function() {
      # do nothing
    },

    updatePriority = function() {
      self$dt[, "priorityAbs"] =  abs(self$dt[, "delta"]) + self$smooth
      self$dt[, "priorityRank"] = order(self$dt[, "delta"])
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
ReplayMem$extractDone = function(x) {
  return(x[[5L]])
}

#' @export
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


#' @export
ReplayMemLatest = R6Class("ReplayMemLatest",
  inherit = ReplayMem,
  public = list(
   sample.fun = function(k) {
      # k is always set to the episode length currently
      k = min(k, self$len)  # when k is too small, the learning stops at particular step
      self$replayed.idx = (self$len - k + 1L): self$len
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    },

    afterStep = function() {
      # do nothing
    },

    afterEpisode = function() {
    }
    ),
  private = list(),
  active = list()
  )

#' @export
ReplayMemLatestProb = R6Class("ReplayMemLatestProb",
  inherit = ReplayMem,
  public = list(
   sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample(self$len, prob = 1L:self$len, size = k)
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    },

   afterEpisode = function() {
      self$updateDT()
    }
    ),
  private = list(),
  active = list()
  )

#' @export
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

#' @export
ReplayMemPrioritizedRank = R6Class("ReplayMemPrioritizedRank",
  inherit = ReplayMem,
  public = list(
    sample.fun = function(k) {
      k = min(k, self$len)
      if (any(is.na(self$dt$priorityRank))) self$replayed.idx = sample.int(self$len)
      else self$replayed.idx = sample.int(self$len, prob = self$dt$priorityRank)[1L:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    },

    afterEpisode = function() {
      self$updateDT()
    },

    afterStep = function() {
    }
    ),
  private = list(),
  active = list()
  )

ReplayMem$factory = function(name, agent, conf) {
  all = getNamespaceExports("rlR")
  mem.idx = which(sapply(all, function(x) grepl("ReplayMem", x)))
  assert(paste0("ReplayMem", name) %in% all[mem.idx])
  tex = sprintf("ReplayMem%s$new(agent = agent, conf = conf)", name)
  mem = eval(parse(text = tex))
  return(mem)
}
