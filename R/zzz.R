#' @importFrom R6 R6Class
#' @import data.table
#' @import checkmate
#' @import data.table
#' @import reticulate
#' @import keras
#' @import logging
#' @import openssl
#' @import ggplot2
NULL
set.seed(0)

#' @export
listClass = function(name = NULL) {
  if (is.null(name)) return(c("Agent", "Policy", "ReplayMem"))
  all = getNamespaceExports("rlR")
  mem.idx = which(sapply(all, function(x) grepl(name, x)))
  all[mem.idx]
}
