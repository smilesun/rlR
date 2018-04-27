#' @title Configuration object for experiment.
#'
#' @description The reason that there exist a Configuration object which is throughout the experiment is that we want to look at the effect of those configuration parameters.
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
RLConf = R6Class("Conf",
  public = list(
    static = NULL,
    initialize = function(...) {
      if (length(list(...)) == 0L) {
        self$static = data.table::copy(rlR.conf.default)  # deep copy
      } else {
        self$set(...)
      }
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

