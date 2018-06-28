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
      if (flag_gym) keras::install_keras(tensorflow = '1.8-gpu')
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
  c("AgentDQN:Deep Q learning", "AgentFDQN:Frozen Target Deep Q Learning", "AgentDDQN: Double Deep QLearning", "AgentPG: Policy Gradient Monte Carlo", "AgentPGBaseline: Policy Gradient with Baseline", "AgentActorCritic: Actor Critic Method", "AgentDDPG: Deep Deterministic Policy Gradient for Continous Action")
}

#' @title listAvailEnvs
#' @description List all environments
#' @export
listAvailEnvs = function() {
  envs = import("gym.envs")
  all_spec = envs$registry$env_specs
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
rlR.xd = function() reticulate::use_python("~/anaconda3/bin/python")
