RLConf = R6Class("Conf",
  public = list(
    static = NULL,
    initialize = function() {
      self$takeStatic()
    },
    takeStatic = function() {
      self$static = data.table::copy(RLConf$static)
    },
    updatePara = function(str.namespace, str.para, val.value) {
      self$static[[str.namespace]][[str.para]] = val.value 
    },
    dict = list()
  ),
  private = list(),
  active = list()
  )

ps = function(temp) {
  for(name in names(temp))
  {
    # cat(sprintf("%s:%s\n", name, toString(temp[[name]])))
    val = sprintf("%s", temp[[name]])
    cat(sprintf("namespace: %s\n%s\n", name, val))

  }
}


RLConf$show = function() {
  temp = RLConf$static
  print(temp)
}

RLConf$update = function(str.namespace, str.para, val.value) {
   RLConf$static[[str.namespace]][[str.para]] = val.value }

RLConf$register = function(namespace) {  # register namespace
  if(namespace %in% names(RLConf$static)) return(FALSE)
  RLConf[[namespace]] = list()
  return(TRUE)
}

RLConf$attach = function(namespace, list.input) {
  if(namespace %in% names(RLConf$static)) return(FALSE)
  RLConf[[namespace]] = list.input
  return(TRUE)
}

RLConf$fetchConf = function(namespace) {
  if(namespace %nin% names(RLConf$static))  stop("configuration namespace does not exist")
  if(is.null(RLConf$static[[namespace]])) stop("configuration namespace empty!")
  return(RLConf$static[[namespace]])
}

