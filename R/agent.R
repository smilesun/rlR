## components of agents
# state preprocessing
# explorer, e.g. epsilon.greedy
# policy
# interact with environment
# value.function (optional)
# type of fun.approx, e.g. neural network
# predict predict values given state (depends on fun.approx)
# train/fit/learn (depends on fun.approx and learner)
# episodic / continuous (part of env?)

## algorithm
# 1. interact
# 2. learn (optional)
# 1. and 2. include often
# - getPreprocessedState: s -> phi (preprocess function)
# - getActionValues: phi -> Q (predict)
# - getPolicy: Q -> pi (e.g. epsilon-greedy)
# - getAction pi -> action (sample from pi)
# - ... (add2Replay, sampleFromReplay ...)
# a reset when episode is done

Agent = R6::R6Class("Agent",
  public = list(
    policy = NULL, # maybe active field
    action = NULL,
    initialize = function() {

    }
  )
)

valueAgent = R6::R6Class("valueAgent",
  inherit = agent,
  public = list(
    value.fun = NULL
  )
)

qLearning = R6::R6Class(
  inherit = valueAgent,
  public = list(
    learn = function() {
      # update action values
    }
  )
)

# testAgent = agent$new()
# testAgent$getPolicy()

# target policy, behaviour policy?

# maybe interaction / experiment class
interaction = R6::R6Class(environment, agent)

# exploration: e.g. epsilon-greedy

# policyGradientAgent

learner = R6::R6Class()




# rl.exp = experiment(task, agent)

# getPolicy(value.fun) -> returns policy, e.g. softmax, epsilon-greedy, random policy
# sampleAction(policy) -> returns action sampled from policy
# interact(agent, environment) -> samples action from policy, then calls step with action
# learn function, learner specific, e.g. qlearning, sarsa: updates weights of value fun or policy

# keras neural network
# eligibility traces, exp. replay ...

## keras mountain car example

