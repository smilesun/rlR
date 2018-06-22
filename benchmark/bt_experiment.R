# addProblem, addAlgorithm, addExperiments(algo.design = ades, repls = REPLS)
source("bt_conf.R")
pp = readline("Are you really sure to delete the registry and restart? Y OR N")
if (pp == "Y") unlink(gbtconf$REG_FILE_DIR, recursive = TRUE, force = TRUE)
reg = batchtools::makeExperimentRegistry(file.dir = gbtconf$REG_FILE_DIR,
  source = c(gbtconf$preSource),
  packages = gbtconf$prePackage,
  seed = gbtconf$SEED_REGISTRY)



lapply(gbtconf$prePackage, require, character.only = TRUE)
lapply(gbtconf$preSource, source)

# Cartesian product
#des = expand.grid(lrn.cl = c("1", "2"), ft.extract.method = c("A", "B"), stringsAsFactors = FALSE)

pdes = list()
for (prob in gbtconf$PROB_RUN) {
  addProblem(name = prob, data = list(iteration = gbtconf$iteration), fun = get(gbtconf$PROB_LIST[[prob]]$fun), seed = gbtconf$SEED_ADDPROBLEM)
  pdes[[prob]] = data.frame(s.name = gbtconf$PROB_LIST[[prob]]$prob.data, stringsAsFactors = FALSE)
}

gbtconf$ALGO_LIST = list()


#gbtconf$ALGO_LIST$rl_algo_dqn = list(fun = rl_algo, design = data.frame(agent.name = gbtconf$agent.name, stringsAsFactors = FALSE))
gbtconf$ALGO_LIST$rl_algo_dqn = list(fun = rl_algo_dqn)
gbtconf$ALGO_LIST$rl_algo_fdqn = list(fun = rl_algo_fdqn)
gbtconf$ALGO_LIST$rl_algo_ddqn = list(fun = rl_algo_ddqn)
gbtconf$ALGO_LIST$rl_algo_pg = list(fun = rl_algo_pg)
gbtconf$ALGO_LIST$rl_algo_pgb = list(fun = rl_algo_pgb)
gbtconf$ALGO_LIST$rl_algo_pgac = list(fun = rl_algo_pgac)


ades = list()

for (algo in gbtconf$ALGO_RUN) {
  addAlgorithm(name = algo, fun = gbtconf$ALGO_LIST[[algo]]$fun)
  # ades[[algo]] = gbtconf$ALGO_LIST[[algo]]$design
}
addExperiments(prob.design = pdes, algo.design = NULL, repls = gbtconf$REPLS)
unwrap(getJobPars())
