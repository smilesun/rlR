# Configuration for benchmarking with batchtools: only one global conf variable
gbtconf = list()

## Dependencies
gbtconf$preSource = c("bt_algorithms.R", "bt_conf.R", "bt_problem.R")
gbtconf$prePackage = c("batchtools", "checkmate", "aslib", "data.table", "R6", "reticulate", "keras", "logging", "BBmisc", "openssl", "ggplot2", "reshape2", "rlR")

## EVALUATION
gbtconf$SEED_REGISTRY     = 1273L                                 # global seed for reg
gbtconf$SEED_ADDPROBLEM   = 1L                                    # seed for each problem
gbtconf$REPLS = 1L

rlR::listClass()
rlR::listClass("Policy")
rlR::listClass("ReplayMem")
rlR::listClass("Agent")
gbtconf$agent.name = c("AgentDQN", "AgentFDQN", "AgentDDQN")
gbtconf$replay = c("ReplayMemUniform", "ReplayMemLatest", "ReplayMemPrioritizedRank")
gbtconf$policy = c("PolicyEpsilonGreedy", "PolicyProbEpsilon")

## Experiment
gbtconf$REG_FILE_DIR = "bt_reg"
gbtconf$ALGO_RUN = c("rl_algo")
gbtconf$PROB_RUN = c("rl_prob")
gbtconf$PROB_LIST = list()
gbtconf$PROB_LIST[["rl_prob"]] = list(fun = "rl_prob", prob.data = c("MountainCar-v0", "CartPole-v0", "Amidar-ram-v0", "WizardOfWor-ram-v0", "Asteroids-ram-v0", "KungFuMaster-ram-v0", "JourneyEscape-ram-v-ram-v0")
)
gbtconf$iteration = 500L
