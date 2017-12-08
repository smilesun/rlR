PolicyFactory = R6Class("PolicyFactory")
PolicyFactory$epsilonPolicy = function(state = NULL, host) {
      if(runif(1L) < host$epsilon) {
        host$sampleRandomAct()
        log.nn$info("random chosen %d \n\n", host$random.action)
        return(TRUE)
      }
      return(FALSE)
    }

PolicyFactory$greedyPolicy = function(state, host) {
      action = which.max(host$vec.arm.q) - 1L  # always use OpenAI gym convention
      log.nn$info("chosen %d \n\n", action)
      return(action)
    }

PolicyFactory$policy.epsilonGreedy = function(state, host) {
      action = PolicyFactory$greedyPolicy(state, host)
      if(PolicyFactory$epsilonPolicy(state, host)) {
        action = host$random.action
      }
      return(action)
    }


PolicyFactory$static = list(
  "epsilon" = PolicyFactory$epsilonPolicy,
  "greedy" =  PolicyFactory$greedyPolicy,
  "epsilonGreedy" = PolicyFactory$policy.epsilonGreedy
)

PolicyFactory$make = function(name, host) {
  if(name %nin% names(PolicyFactory$static)) stop("no such policy!")
  function(state) PolicyFactory$static[[name]](state, host)
}

