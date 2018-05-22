# The reason that there exist a Configuration object which is throughout the experiment is that we want to look at the effect of those configuration parameters.
RLConf = R6Class("Conf",
  public = list(
    static = NULL,
    perf.path = NULL,
    conf.log.perf = NULL,
    getPersist = function() {
      self$conf.log.perf = data.table::copy(rlR:::rlR.conf4log)
      list.str = lapply(names(self$static), function(x) sprintf("-%s: %s-\n", x, self$static[[x]]))
      self$conf.log.perf$str.conf = paste0("\n", toString(list.str))
      hash.conf = openssl::md5(self$conf.log.perf$str.conf)
      str.time = toString(Sys.time())
      str.time = gsub(" ", "_", str.time)
      str.date = toString(Sys.Date())
      filePrefix = file.path(getwd(), self$conf.log.perf$ROOTFOLDERNAME, str.date, str.time, hash.conf)
      #cat(sprintf("logout file path %s", filePrefix))
      self$conf.log.perf$filePrefix = filePrefix
      self$conf.log.perf$resultTbPath =  file.path(filePrefix, rlR.conf4log$resultTbPath)  # RData file persistence place
    },

    initialize = function(...) {
      self$static = data.table::copy(rlR.conf.default)  # deep copy
      par.list = list(...)
      dns = setdiff(names(par.list), rlR.conf.default)
      list.default = setNames(lapply(dns, function(x) self$static[[x]]), dns)
      self$set(...)
      self$getPersist()
      cat("parameters: \n")
      dns = names(self$static)
      lapply(dns, function(x) cat(sprintf("-%s: %s- %s-\n", x, {
            ns = names(self$static[[x]])
            if (is.null(ns)) ns = ""
            ns
        }, self$static[[x]])))
    },

    get = function(name) {
      self$static[[name]]
    },

    set = function(...) {
      par.list = list(...)
      lapply(names(par.list), function(x) self$updatePara(x, par.list[[x]]))
    },

    updatePara = function(str.para, val.value) {
      self$static[[str.para]] = val.value
    },

    show = function() {
      temp = self$static
      print(temp)
    },

    ps = function() {
      for (name in names(self$static)) {
        val = sprintf("%s", self$static[[name]])
        cat(sprintf("namespace: %s\n%s\n", name, val))
      }
    },

    register = function(namespace) {
      # register namespace
      if (namespace %in% names(self$static)) return(FALSE)
      self$static[[namespace]] = list()
      return(TRUE)
    },

    attachname = function(namespace, list.input) {
      # attach a new subspace of parameters
      if (namespace %in% names(self$static)) return(FALSE)
      self$static[[namespace]] = list.input
      return(TRUE)
    },

    fetchConf = function(namespace) {
      if (namespace %nin% names(self$static))  stop("configuration namespace does not exist")
      if (is.null(self$static[[namespace]])) stop("configuration namespace empty!")
      return(self$static[[namespace]])
    },

    dict = list()
  ),
  private = list(),
  active = list()
  )
