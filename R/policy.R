#' @title Policy method
#'
#' @description Policy method
#'
#' @param state currently not used at all
#' @param host value
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
PolicyFactory = R6Class("PolicyFactory")

PolicyFactory$epsilonPolicy = function(state = NULL, host) {
      if (runif(1L) < host$epsilon) {
        host$sampleRandomAct()
        return(TRUE)
      }
      return(FALSE)
    }

PolicyFactory$greedyPolicy = function(state, host) {
      action = which.max(host$vec.arm.q)  # always use OpenAI gym convention
      return(action)
    }

PolicyFactory$policy.epsilonGreedy = function(state, host) {
      action = PolicyFactory$greedyPolicy(state, host)
      if (PolicyFactory$epsilonPolicy(state, host)) {
        action = host$random.action
        host$random.cnt = host$random.cnt + 1L  # increment random count
        host$glogger$log.nn$info("random action: %d", action)
      }
      return(action)
    }

# all suboptimal arm probability sum up to epsilon with probability epsilon/actCnt
PolicyFactory$probEpsilon = function(state, host) {
      prob = rep(host$epsilon, host$actCnt) / (host$actCnt)
      optarm = which.max(host$vec.arm.q)
      prob[optarm] = prob[optarm] + 1.0 - host$epsilon
      action  = sample.int(host$actCnt, prob = prob)[1L]
      return(action)
}

#
PolicyFactory$policy.predProbRank = function(state, host) {
      prob = order(host$vec.arm.q)
      action = sample.int(host$actCnt, prob = prob)[1L]
      return(action)
    }
# softmax will magnify the difference
PolicyFactory$policy.predsoftmax = function(state, host) {
      prob = exp(+1 * host$vec.arm.q)
      prob = prob / sum(prob)
      action = sample.int(host$actCnt, prob = prob)[1L]
      return(action)
    }

PolicyFactory$make0 = function(name, host) {
  if (name %nin% names(PolicyFactory)) stop("no such policy!")
  function(state) PolicyFactory[[name]](state, host)
}

PolicyFactory$make = function(name, host) {
  fn = paste0("Policy", name)
  if (fn %nin% listClass("Policy")) stop("no such policy!")
  return(eval(parse(text = sprintf("%s$new(host = host)", fn))))
}

#' @export
Policy = R6Class("Policy",
  public = list(
    host = NULL,
    minEpsilon = 0.01,
    maxEpsilon = 1,
    initialize = function(host) {
      self$host = host
    },

    afterEpisode = function() {
    }
  )
  )

#' @export
PolicyEpsilonGreedy = R6Class("PolicyEpsilonGreedy",
  inherit = Policy,
  public = list(
    act = function(state) {
      PolicyFactory$policy.epsilonGreedy(state, self$host)
    },

    afterStep = function() {
      self$host$epsilon =  self$minEpsilon + (self$maxEpsilon - self$minEpsilon) * exp(-0.001 * self$host$gstep.idx)
    },

    afterEpisode = function() {
      self$host$decayEpsilon()
    }
    )
  )

#' @export
PolicyProbEpsilon = R6Class("PolicyProbEpsilon",
  inherit = PolicyEpsilonGreedy,
  public = list(
    act = function(state) {
      PolicyFactory$probEpsilon(state, self$host)
    },

    afterEpisode = function() {
      self$host$decayEpsilon()
    }
    )
  )

PolicyPG = R6Class("PolicyProbEpsilon",
  inherit = PolicyEpsilonGreedy,
  public = list(
    act = function(state) {
      action = PolicyFactory$policy.predsoftmax(state, self$host)
      if (PolicyFactory$epsilonPolicy(state, self$host)) {
        action = self$host$random.action
        self$host$random.cnt = self$host$random.cnt + 1L  # increment random count
        self$host$glogger$log.nn$info("random action: %d", action)
      }
      action
    },

    afterEpisode = function() {
      self$host$decayEpsilon()
    }
    )
  )
