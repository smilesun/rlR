## ----setup, include = FALSE, cache = FALSE-------------------------------
library(rlR)
set.seed(123)
knitr::opts_chunk$set(cache = TRUE, collapse = FALSE, dev = "svg", fig.height = 3.5)
knitr::knit_hooks$set(document = function(x){
  gsub("```\n*```r*\n*", "", x)
})
library(reticulate)

## ----eval=FALSE----------------------------------------------------------
#  reticulate::py_discover_config()

## ----eval=FALSE----------------------------------------------------------
#  Sys.which("python")

## ----eval=FALSE----------------------------------------------------------
#  reticulate::use_python("/usr/bin/python", required=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  reticulate::py_config()

## ----eval=F--------------------------------------------------------------
#  rlR::installDep2SysVirtualEnv(gpu = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  rlR::installDepConda(conda_path = "auto", gpu = FALSE)

