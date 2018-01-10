PolicyFactory = R6Class("PolicyFactory")
PolicyFactory$epsilonPolicy = function(state = NULL, host) {
      if(runif(1L) < host$epsilon) {
        host$sampleRandomAct()
        return(TRUE)
      }
      return(FALSE)
    }

PolicyFactory$greedyPolicy = function(state, host) {
      action = which.max(host$vec.arm.q) - 1L  # always use OpenAI gym convention
      return(action)
    }

PolicyFactory$policy.epsilonGreedy = function(state, host) {
      action = PolicyFactory$greedyPolicy(state, host)
      if(PolicyFactory$epsilonPolicy(state, host)) {
        action = host$random.action
      }
      return(action)
    }

PolicyFactory$policy.predProbRank = function(state, host) {
      action = PolicyFactory$greedyPolicy(state, host)
      prob = order(host$vec.arm.q)
      sample.int(host$actCnt, prob = prob)[1L] -  1L
      return(action)
    }

PolicyFactory$static = list(
  "epsilon" = PolicyFactory$epsilonPolicy,
  "greedy" =  PolicyFactory$greedyPolicy,
  "epsilonGreedy" = PolicyFactory$policy.epsilonGreedy,
  "policy.predProbRank" = PolicyFactory$policy.predProb
)

PolicyFactory$make = function(name, host) {
  if(name %nin% names(PolicyFactory$static)) stop("no such policy!")
  function(state) PolicyFactory$static[[name]](state, host)
}

# A smarter epsilon policy sum[1/N*epsilon,...] = N* 1/N*epsilon = epsilon, i.e. all non optimal action take probability epsilon
#      def policy_fn(sess, observation, epsilon):
#         A = np.ones(nA, dtype=float) * epsilon / nA
#         q_values = estimator.predict(sess, np.expand_dims(observation, 0))[0]
#         best_action = np.argmax(q_values)
#         A[best_action] += (1.0 - epsilon)
#         return A
# 
