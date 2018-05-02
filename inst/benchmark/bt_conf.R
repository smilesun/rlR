# Configuration for benchmarking with batchtools: only one global conf variable
gbtconf = list()

## Dependencies
gbtconf$preSource = c("bt_algorithms.R", "bt_conf.R", "bt_problem.R")
gbtconf$preSource.org = c("conf.R", "nnArsenal.R", "conf_static.R", "environment_base.R", "environment_gym.R", "interaction_base.R", "interaction_observer.R", "interaction_gym.R", "logging.R", "replaymem.R", "stl.R", "performance.R", "agentFactory.R", "agent_base.R", "agent_dqn.R", "agent_actor_critic.R", "agent_pg.R", "policy.R", "surrogate_base.R", "surrogate_dqn.R", "surrogate_mlr.R", "examples.R", "experiment.R")

gbtconf$prePackage = c("checkmate", "aslib", "data.table", "R6", "reticulate", "keras", "logging", "BBmisc", "openssl", "ggplot2", "reshape2")

## EVALUATION
gbtconf$SEED_REGISTRY     = 1273L                                 # global seed for reg
gbtconf$SEED_ADDPROBLEM   = 1L                                    # seed for each problem
gbtconf$REPLS = 1L


## Experiment
gbtconf$REG_FILE_DIR = "bt_reg"
gbtconf$ALGO_RUN = c("rl_algo")
gbtconf$PROB_RUN = c("rl_prob")
gbtconf$PROB_LIST = list()
gbtconf$prob.data = list()
gbtconf$PROB_LIST[["rl_prob"]] = list(fun = "rl_prob", prob.data = gbtconf$prob.data)

