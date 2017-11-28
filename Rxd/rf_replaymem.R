ReplayMem = R6Class("ReplayMem",
  public = list(
    samples = NULL,
    dt = NULL,
    len = 0,
    sample.fun = function(){},
    list.sample.fun = list(),
    initialize = function(name) {
      self$samples = list()
      self$dt = data.table()
      self$len = 0
      # self$list.sample.fun = list("uniform-all":self$ins.sample, "latest-fix":self$ins.sample.latest, "all": self$ins.sample.all)
    },

    add = function(ins) {
      len = length(self$samples)
      self$samples[[len + 1]] = ins
      self$len = self$len + 1
      self$dt = rbindlist(list(self$dt, list(unlist(ins))))
    },

    ins.sample = function(k) {
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

ReplayMemUniform = R6Class("ReplayMem",
  inherit = ReplayMem,
  public = list(
    initialize = function(name ="uniform-all") {
      super$initialize(name)
    },
    sample.fun = function(k) {
      k = min(k, self$len)
      list.res = lapply(sample(self$len)[1:k], function(x) self$samples[[x]])
      return(list.res)
    }
    ),
  private = list(),
  active = list()
  )


ReplayMemLatest = R6Class("ReplayMem",
  inherit = ReplayMem,
  public = list(
    initialize = function(name ="uniform-all") {
      super$initialize(name)
    },
    sample.fun = function(k) {
      k = min(k, self$len)
      x = (self$len - k + 1L): self$len
      list.res = lapply(x, function(x) self$samples[[x]])
      return(list.res)
    }
    ),
  private = list(),
  active = list()
  )


ReplayMem$factory = function(name) {
  hash = list("uniform" = ReplayMemUniform, "latest" = ReplayMemLatest)
  return(hash[[name]]$new)

}


