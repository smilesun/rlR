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
      prob = order(host$vec.arm.q)
      action = sample.int(host$actCnt, prob = prob)[1L] -  1L
      return(action)
    }

# softmax will magnify the difference 
PolicyFactory$policy.predsoftmax = function(state, host) {
      prob = exp(host$vec.arm.q)
      prob = prob / sum(prob)
      action = sample.int(host$actCnt, prob = prob)[1L] -  1L
      return(action)
    }

# all suboptimal arm probability sum up to epsilon with probability epsilon/actCnt
PolicyFactory$probEpsilon = function(state, host) {
      prob = rep(RLConf$static$agent$fixedEpsilon, host$actCnt)/(host$actCnt)
      optarm = which.max(host$vec.arm.q)
      prob[optarm] = prob[optarm] + 1.0 - RLConf$static$agent$fixedEpsilon
      action  = sample.int(host$actCnt, prob = prob)[1L] -  1L
      return(action)
}

PolicyFactory$static = list(
  "greedy" =  PolicyFactory$greedyPolicy,
  "epsilonGreedy" = PolicyFactory$policy.epsilonGreedy,
  "policy.predProbRank" = PolicyFactory$policy.predProbRank,
  "probEpsilon" = PolicyFactory$probEpsilon,
  "softmax" = PolicyFactory$policy.predsoftmax
)

PolicyFactory$make = function(name, host) {
  if(name %nin% names(PolicyFactory$static)) stop("no such policy!")
  function(state) PolicyFactory$static[[name]](state, host)
}

