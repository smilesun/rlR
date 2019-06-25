## ----setup, include = FALSE, cache = FALSE-------------------------------
library(rlR)
set.seed(123)
knitr::opts_chunk$set(cache = TRUE, collapse = FALSE, dev = "svg", fig.height = 3.5)
knitr::knit_hooks$set(document = function(x){
  gsub("```\n*```r*\n*", "", x)
})
library(reticulate)
os = import("os") 
os$environ[["TF_CPP_MIN_LOG_LEVEL"]]="3" 

## ------------------------------------------------------------------------
help(topic="Environment", package = "rlR")

## ------------------------------------------------------------------------
env = rlR:::EnvToy$new()

## ------------------------------------------------------------------------
class(env)

## ------------------------------------------------------------------------
env$initialize  # the fields 'act_cnt' and  'state_dim' must be defined here

## ------------------------------------------------------------------------
env$reset  # The return must be a  list with fields state(must be an array), reward = NULL, done = FALSE, and info = list()

## ------------------------------------------------------------------------
env$step  # The return must be a list with fields state(must be an array), reward(numeric), done(Boolean), and info (list of anything or empty list)

## ------------------------------------------------------------------------
agent = initAgent("AgentDQN", env)
agent$learn(3)

