#' @import R6
#' @import data.table
#' @import checkmate
#' @import data.table
#' @import reticulate
#' @import keras
#' @import logging
#' @import openssl
#' @import ggplot2
NULL


#' @title Test if tensorflow works from R session
#'
#' @description Test if tensorflow works from R session
#'
#' @return TRUE if tensorflow works
#' @export
rlr_test_if_tensorflow_works = function() {
  res <- try({
    tf = reticulate::import("tensorflow")
    sess = tf$Session()
    hello = tf$constant('Hello, TensorFlow!')
    sess$run(hello)
  }, silent = FALSE)
  if (class(res)[1L] == "try-error") return(FALSE)
  return(TRUE)
  #tensorflow::install_tensorflow()
}

#' @title  Test if keras works
#' 
#' @description Test if keras is installed
#' 
#' @return TRUE if success
#' @export 
rlr_test_if_keras_works = function() {
  res <- try({
      makeAnyModel(input = 4, output = 1, list.arch = list.arch)
  }, silent = FALSE)
  if (class(res)[1L] == "try-error") return(FALSE)
  return(TRUE)
}

#' @title  Test if gym is installed
#' 
#' @description Test if gym is installed
#' 
#' @return TRUE if success
#' @export 
rlr_test_if_gym_works = function() {
  res <- try({
    gym = reticulate::import("gym")
    gym$logger$set_level(40)  # supress warning
    gym$logger$setLevel(40)
    genv = gym$make("CartPole-v0")
  }, silent = FALSE)
  if (class(res)[1L] == "try-error") return(FALSE)
  return(TRUE)
}

#' @title  Install dependencies
#' @param gpu Wehter to use gpu tensorflow or not
#' @description Install Keras dependencies, if dependencies already installed, will not re-install
#' @return NULL
#' @export
installDep = function(gpu = FALSE) {
  flag_keras = rlr_test_if_keras_works()
  flag_gym = rlr_test_if_gym_works()
  if (gpu) {
    if (!flag_keras) {
      if (flag_gym) keras::install_keras(tensorflow ='1.8-gpu')
      else keras::install_keras(tensorflow = "1.8-gpu", extra_packages = c("gym"))
    }
  }
  else {
  if (!flag_keras) {
    if (flag_gym) keras::install_keras(tensorflow = "1.8.0")
    else keras::install_keras(tensorflow = "1.8.0", extra_packages = c("gym"))
  }
  }
}

#' @title listAvailAgent
#' @description List all implemented Agents
#' @export
listAvailAgent = function() {
  c("AgentDQN:deep q learning", "AgentFDQN:frozen target deep q learning", "AgentDDQN: double deep q learning", "AgentPG: policy gradient basic", "AgentPGBaseline: policy gradient with baseline", "AgentActorCritic: actor critic method")
}


# listClass = function(name = NULL) {
#   if (is.null(name)) return(c("Agent", "Policy", "ReplayMem"))
#   all = getNamespaceExports("rlR")
#   mem.idx = which(sapply(all, function(x) grepl(name, x)))
#   all[mem.idx]
# }
rlR.xd = function() reticulate::use_python("~/anaconda3/bin/python")
rlR.xd()
