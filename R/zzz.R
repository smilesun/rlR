#' @import R6
#' @import data.table
#' @import checkmate
#' @import data.table
#' @import reticulate
#' @import keras
#' @import logging
#' @import openssl
#' @import ggplot2
#' @import tensorflow
#' @import abind
#' @import foreach

NULL # nocov

.onAttach <- function(libname, pkgname) {
   try(expr = {
   packageStartupMessage("- type 'reticulate::py_discover_config()' to check default python")
   packageStartupMessage("- to use a different python path, execute the following immediately after package is loaded:")
   packageStartupMessage("reticulate::use_python('/path/to/your/python')")
   packageStartupMessage("\nor\n reticulate::use_conda_env('name-of-conda-env')")
   }, silent = TRUE)
}

#' @title List implemented Agents
#' @description List all implemented Agents
#' @export
listAvailAgent = function() {
  all = getNamespaceExports("rlR")
  all =  all[which(sapply(all, function(x) grepl("^Agent", x)))]
  kickout = c("Agent", "AgentArmed")
  all = setdiff(all, kickout)
  list_res = lapply(all, function(x) get(x)$info())
  names(list_res) = all
  list_res
}

#' @title list environments from OPENAI gym
#' @description List all Gym Environments without testing them
#' @export
listGymEnvs = function() {
  envs = reticulate::import("gym.envs")
  all_spec = envs$registry$env_specs
  res = sapply(all_spec, function(x) x$id)
  names(res) = NULL
}


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
    hello = tf$constant("Hello, TensorFlow!")
    sess$run(hello)
  }, silent = FALSE)
  if (class(res)[1L] == "try-error") return(FALSE)
  return(TRUE)
}

#' @title  Test if gym is installed
#' @description Test if gym is installed
#' @return TRUE if success
#' @export
rlr_test_if_gym_works = function() {
  res <- try({
    gym = reticulate::import("gym")
    gym.sp = reticulate::import("gym.spaces")
    gym$logger$set_level(40)  # supress warning
    gym$logger$setLevel(40)
    genv = gym$make("CartPole-v0")
    genv$reset()
  }, silent = FALSE)
  if (class(res)[1L] == "try-error") return(FALSE)
  return(TRUE)
}

#' @title  Check if python dependencies work
#' @description Check if python dependencies work
#' @return TRUE if all python dependencies work
#' @export
checkPyDep = function() {
  flag_tensorflow = rlr_test_if_tensorflow_works()
  flag_keras = rlr_test_if_keras_works()
  flag_gym = rlr_test_if_gym_works()
  cat(sprintf("\n tensorlfow: %s, keras: %s, gym:%s\n", flag_tensorflow, flag_keras, flag_gym))
  return(flag_tensorflow && flag_keras && flag_gym)
}

#' @title  Install dependencies into system virtual environment called r-tensorflow
#' @param gpu If TRUE, will install gpu version of tensorflow. By default, FALSE
#' @description Install Keras dependencies into system virtual environment called r-tensorflow
#' @return NULL
#' @export
installDep2SysVirtualEnv = function(gpu = FALSE) {  # nocov start
  cat(sprintf("\ninstalling dependencies using %s \n",  Sys.which("virtualenv")))
  # install_keras will install tensorflow along into the virtual environment called "r-tensorflow"
  if (gpu) {
    version = paste0("1.8.0", "-gpu")
  } else {
    version = "1.8.0"
  }
  keras::install_keras(method = "virtualenv", tensorflow = version, extra_packages = c("gym==0.10.5", "cmake==3.12.0", "atari-py==0.1.6"))
  #reticulate::py_install()
  # sudo pip instlal uwsgi
  # sudo apt-get install python3-pip
} # nocov end

#' @title  Install dependencies into a conda virtual environment called r-tensorflow
#' @param gpu If TRUE, will install gpu version of tensorflow. By default, FALSE
#' @param conda_path The conda path in your system, default "auto" will search in system path
#' @description Install Keras dependencies into a conda virtual environment called r-tensorflow
#' @return NULL
#' @export
installDepConda = function(conda_path = "auto", gpu = FALSE) { # nocov start
  str4gpu = ifelse(gpu, "-gpu", "")
  if (conda_path == "auto") cat(sprintf("\ninstalling dependencies using %s \n", Sys.which("conda")))
  tf_version = paste0("1.9.0", str4gpu)
  keras_version = "default"
  keras::install_keras(method = "conda", conda = conda_path, version = keras_version, tensorflow = tf_version, extra_packages = c("gym==0.10.5",  "cmake==3.12.0", "atari-py==0.1.6"))
} # nocov end


#' @title  Test if keras works
#' @description Test if keras is installed
#' @return TRUE if success
#' @export
rlr_test_if_keras_works = function() {
  requireNamespace("keras")
  res <- try({
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')
  }, silent = FALSE)
  if (class(res)[1L] == "try-error") return(FALSE)
  return(TRUE)
}

rlR.debug = FALSE  # nocov
