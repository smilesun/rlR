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
installDep2SysVirtualEnv = function(gpu = FALSE) {
  cat(sprintf("\ninstalling dependencies using %s \n",  Sys.which("virtualenv")))
  # install_keras will install tensorflow along into the virtual environment called "r-tensorflow"
  if (gpu) keras::install_keras(method = "virtualenv", tensorflow = "1.8.0-gpu", extra_packages = c("gym"))
  else keras::install_keras(method = "virtualenv", tensorflow = "1.8.0", extra_packages = c("gym"))
  #reticulate::py_install()
  # sudo pip instlal uwsgi
  # sudo apt-get install python3-pip
}

#' @title  Install dependencies into a conda virtual environment called r-tensorflow
#' @param gpu If TRUE, will install gpu version of tensorflow. By default, FALSE
#' @param conda_path The conda path in your system, default "auto" will search in system path
#' @description Install Keras dependencies into a conda virtual environment called r-tensorflow
#' @return NULL
#' @export
installDepConda = function(conda_path = "auto", gpu = FALSE) {
  if (conda_path == "auto") cat(sprintf("\ninstalling dependencies using %s \n", Sys.which("conda")))
  # install_keras will install tensorflow along into the virtual environment called "r-tensorflow"
  if (gpu) keras::install_keras(method = "conda", conda = conda_path, tensorflow = "1.8.0-gpu", extra_packages = c("gym"))
  else keras::install_keras(method = "conda", conda = conda_path, tensorflow = "1.8.0", extra_packages = c("gym"))
}


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

#' @title listAvailAgent
#' @description List all implemented Agents
#' @export
listAvailAgent = function() {
  c("AgentDQN:Deep Q learning", "AgentFDQN:Frozen Target Deep Q Learning", "AgentDDQN: Double Deep QLearning", "AgentPG: Policy Gradient Monte Carlo", "AgentPGBaseline: Policy Gradient with Baseline", "AgentActorCritic: Actor Critic Method", "AgentDDPG: Deep Deterministic Policy Gradient for Continous Action")
}

#' @title listAvailEnvs
#' @description List all environments
#' @param check Whether to check if each environment works or not, default FALSE
#' @export
listAvailEnvs = function(check = FALSE) {
  envs = import("gym.envs")
  all_spec = envs$registry$env_specs
  if (!check) return(all_spec)
  idx = lapply(all_spec, function(spec) {
    env = try({env = spec$make()})
    if (class(env) == "try-error") return(FALSE)
    flag_discrete = "n" %in% names(env$action_space)
    flag_nonarray = FALSE
    if ("shape" %in% names(env$action_space)) {
      flag_nonarray = length(env$action_space$shape) == 1L
    }
    return(flag_discrete || flag_nonarray)
  })
  lapply(all_spec[which(unlist(idx))], function(x) x$id)
}
#rlR.xd = function() reticulate::use_python("~/anaconda3/bin/python")
