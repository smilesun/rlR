# addProblem, addAlgorithm, addExperiments(algo.design = ades, repls = REPLS)
source("bt_conf.R")
pp = readline("Are you really sure to delete the registry and restart? Y OR N")
if(pp == "Y") unlink(gbtconf$REG_FILE_DIR, recursive = TRUE, force = TRUE)
reg = makeExperimentRegistry(file.dir = gbtconf$REG_FILE_DIR,
  source = c(gbtconf$preSource, gbtconf$preSource.org),
  packages = gbtconf$prePackage,
  seed = gbtconf$SEED_REGISTRY)



lapply(gbtconf$prePackage, require, character.only = TRUE)
lapply(gbtconf$preSource, source)  
lapply(gbtconf$preSource.org, source)  


for(prob in gbtconf$PROB_RUN) {
  addProblem(name = prob, data = gbtconf$PROB_LIST[[prob]]$prob.data, fun = get(gbtconf$PROB_LIST[[prob]]$fun), seed = gbtconf$SEED_ADDPROBLEM)
}


gbtconf$ALGO_LIST = list()

gbtconf$ALGO_LIST$rl_algo = list(fun = rl_algo, design = data.frame(lrn.cl = "nn", stringsAsFactors = FALSE))  # do not add other irrelevant designs here since the algorithm function will not use them

ades = list()

for(algo in gbtconf$ALGO_RUN) {
  addAlgorithm(name = algo, fun = gbtconf$ALGO_LIST[[algo]]$fun)
  ades[[algo]] = gbtconf$ALGO_LIST[[algo]]$design
}

addExperiments(algo.design = ades, repls = gbtconf$REPLS)
