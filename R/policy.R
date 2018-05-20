#' @export
Policy = R6Class("Policy",
  public = list(
    epsilon = NULL,
    decay = NULL,
    logdecay = NULL,
    host = NULL,
    minEpsilon = 0.01,
    maxEpsilon = 1,
    initialize = function(host) {
      self$host = host
      self$decay = self$host$conf$get("policy.decay")
      self$logdecay = log(self$decay)
      self$minEpsilon = self$host$conf$get("policy.minEpsilon")
      self$maxEpsilon = self$host$conf$get("policy.maxEpsilon")
      self$epsilon = self$maxEpsilon
    },

    predProbRank = function(state) {
      prob = order(self$host$vec.arm.q)
      action = sample.int(self$host$actCnt, prob = prob)[1L]
      return(action)
    },

    decayEpsilon = function() {
        temp = self$epsilon * self$decay
        self$epsilon = max(temp, self$minEpsilon)
        self$host$interact$toConsole("Epsilon%f \n", temp)  # same message to console
        self$host$glogger$log.nn$info("rand steps:%i \n", self$host$random.cnt)
        self$host$interact$toConsole("rand steps:%i \n", self$host$random.cnt)  # same message to console
        self$host$random.cnt = 0L
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
      action = which.max(self$host$vec.arm.q)
      if (runif(1L) < self$epsilon) {
        self$host$sampleRandomAct()
        action = self$host$random.action
        self$host$random.cnt = self$host$random.cnt + 1L  # increment random count
        self$host$glogger$log.nn$info("random action: %d", action)
      }
      return(action)
    },

    afterStep = function() {
      self$epsilon =  self$minEpsilon + (self$maxEpsilon - self$minEpsilon) * exp(self$logdecay * self$host$gstep.idx)
    },

    afterEpisode = function() {
      self$decayEpsilon()
    }
    )
  )

#' @export
PolicyProbEpsilon = R6Class("PolicyProbEpsilon",
  inherit = PolicyEpsilonGreedy,
  public = list(

    # all suboptimal arm probability sum up to epsilon with probability epsilon/actCnt
    act = function(state) {
      prob = rep(self$epsilon, self$host$actCnt) / (self$host$actCnt)
      optarm = which.max(self$host$vec.arm.q)
      prob[optarm] = prob[optarm] + 1.0 - self$epsilon
      action  = sample.int(self$host$actCnt, prob = prob)[1L]
      return(action)
    },

    afterEpisode = function() {
      self$decayEpsilon()
    }
    )
  )

PolicyPG = R6Class("PolicyPG",
  inherit = PolicyEpsilonGreedy,
  public = list(

    # softmax will magnify the difference
    softmax = function(state) {
      prob = exp(+1 * self$host$vec.arm.q)
      prob = prob / sum(prob)
      action = sample.int(self$host$actCnt, prob = prob)
      action = rmultinom(n = 1L, size = 1L, prob = prob)  # FIXME: any difference between multinomial and sample.int? 
      arm = which.max(action)
      return(arm)
    },

    act = function(state) {
      action = self$softmax(state)
      if (runif(1L) < self$epsilon) {
        self$host$sampleRandomAct()
        action = self$host$random.action
        self$host$random.cnt = self$host$random.cnt + 1L  # increment random count
        self$host$glogger$log.nn$info("random action: %d", action)
      }
      action
    },

    afterEpisode = function() {
      self$decayEpsilon()
    }
    )
  )

makePolicy = function(name, host) {
  fn = paste0("Policy", name)
  return(eval(parse(text = sprintf("%s$new(host = host)", fn))))
}
