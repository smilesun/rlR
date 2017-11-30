Policy = R6::R6Class("Policy",
  public = list(
    policy = NULL,
    getPolicy = NULL,
    sampleAction = function() {
      action = sample(seq_along(self$policy), prob = self$policy, size = 1, replace = TRUE) - 1L
      action
    },
    initialize = function(type = "epsilon-greedy") {
      self$getPolicy = epsilonGreedyPolicy
    }
  )
)

# get epsilon-greedy policy with respect to Q
# Q is a one-column matrix
epsilonGreedyPolicy = function(Q, epsilon) {
  greedy.action = which.max(Q)
  n.actions = length(Q)
  policy = matrix(0, nrow = 1, ncol = n.actions)
  policy[, greedy.action] = 1 - epsilon
  policy = policy + epsilon / n.actions
  policy
}

# sample action from policy
sampleActionFromPolicy = function(policy) {
  action = sample(seq_along(policy), prob = policy, size = 1, replace = TRUE) - 1L
  action
}
