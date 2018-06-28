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
ReplayMem$extractStep = function(x) {
  return(x[[6L]][["stepidx"]])
}

#FIXME: under development
ReplayMemLatestProb = R6::R6Class("ReplayMemLatestProb",
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

ReplayMemPrioritizedAbs = R6::R6Class("ReplayMemPrioritizedAbs",
  inherit = ReplayMemPrioritizedRank,
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

ReplayMemPrioritizedRank = R6::R6Class("ReplayMemPrioritizedRank",
  inherit = ReplayMem,
  public = list(
    smooth = NULL,
    flag_dt = NULL,
    dt = NULL,
    dt.temp = NULL,
    initialize = function(agent, conf) {
      super$initialize(agent, conf)
      self$dt = data.table()
      self$smooth = rlR.conf4log[["replay.mem.laplace.smoother"]]
      self$flag_dt = self$conf$get("replay.mem.dt")
      if (self$flag_dt) self$initTable(mem)
    },

    add = function(ins) {
      super$add(ins)
      if (self$flag_dt) self$appendDT(ins)
    },

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
    },

    initTable = function() {
      self$dt.temp = data.table("delta" = NA, "priorityRank" = NA, "priorityAbs" = NA, "priorityDelta2" = NA, "deltaOfdelta" = NA, "deltaOfdeltaPercentage" = NA)
      self$dt.temp = self$dt.temp[, lapply(.SD, as.numeric)]
    },

    appendDT = function(ins) {
      mdt = data.table(t(unlist(ins)))
      mdt = cbind(mdt, self$dt.temp)
      self$dt = rbindlist(list(self$dt, mdt), fill = TRUE)
    },

    updateDT = function(idx = NULL) {
      self$agent$getXY(self$len)
      self$dt[idx, "delta"] = as.vector(self$replay_delta)
      self$updatePriority()
    },

    updatePriority = function() {
      self$dt[, "priorityAbs"] =  abs(self$dt[, "delta"]) + self$smooth
      self$dt[, "priorityRank"] = order(self$dt[, "delta"])
    }
    )
  )
