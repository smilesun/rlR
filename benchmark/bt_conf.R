# Configuration for benchmarking with batchtools: only one global conf variable
gbtconf = list()

## Dependencies
gbtconf$preSource = c("bt_algorithms.R", "bt_conf.R", "bt_problem.R")
gbtconf$prePackage = c("batchtools", "checkmate", "data.table", "R6", "reticulate", "keras", "logging", "BBmisc", "openssl", "ggplot2", "reshape2", "rlR")
#gbtconf$prePackage = c("aslib")
## EVALUATION
gbtconf$SEED_REGISTRY     = 1273L                                 # global seed for reg
gbtconf$SEED_ADDPROBLEM   = 1L                                    # seed for each problem
gbtconf$REPLS = 1L

gbtconf$agent.name = c("AgentDQN", "AgentFDQN", "AgentDDQN", "AgentPG", "AgentPGBaseline", "AgentActorCritic")
gbtconf$replay = c("ReplayMemUniform", "ReplayMemLatest", "ReplayMemPrioritizedRank")
gbtconf$policy = c("PolicyEpsilonGreedy", "PolicyProbEpsilon")

## Experiment
gbtconf$REG_FILE_DIR = "bt_reg_new"
gbtconf$ALGO_RUN = c("rl_algo_dqn", "rl_algo_ddqn", "rl_algo_fdqn", "rl_algo_pg", "rl_algo_pgb", "rl_algo_pgac")
gbtconf$PROB_RUN = c("rl_prob")
gbtconf$PROB_LIST = list()
#gbtconf$PROB_LIST[["rl_prob"]] = list(fun = "rl_prob", prob.data = c("MountainCar-v0", "CartPole-v0", "Amidar-ram-v0", "WizardOfWor-ram-v0", "Asteroids-ram-v0", "KungFuMaster-ram-v0", "JourneyEscape-ram-v0", "Acrobot-v1")
#gbtconf$PROB_LIST[["rl_prob"]] = list(fun = "rl_prob", prob.data = c("Pong-ram-v0", "CartPole-v0", "Acrobot-v1")
gbtconf$PROB_LIST[["rl_prob"]] = list(fun = "rl_prob", prob.data = c("Pong-ram-v0")
)
gbtconf$iteration = 1000L
