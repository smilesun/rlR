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
library(rlR)
agent = initAgent(name = "AgentTable", env = "CliffWalking-v0")

## ------------------------------------------------------------------------
agent$learn(500)

## ----eval=F--------------------------------------------------------------
#  agent$plotPerf()

