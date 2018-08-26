Policy = R6::R6Class("Policy",
  public = list(
    epsilon = NULL,
    decay_rate = NULL,
    host = NULL,
    min_epsilon = NULL,
    max_epsilon = NULL,
    gstep_idx = NULL,
    action = NULL,
    random_cnt = NULL,
    random_action = NULL,
    softmax_magnify = NULL,
    decay_type = NULL,
    decayEpsilon = NULL,
    total_aneal_step = NULL,
    initialize = function(host) {
      self$random_cnt = 0L
      self$host = host
      self$decay_rate = self$host$conf$get("policy.decay.rate")
      self$min_epsilon = self$host$conf$get("policy.minEpsilon")
      self$max_epsilon = self$host$conf$get("policy.maxEpsilon")
      self$decay_type = self$host$conf$get("policy.decay.type")
      self$total_aneal_step = self$host$conf$get("policy.aneal.steps")
      if (self$decay_type == "decay_geo") self$decayEpsilon = self$decayGeo
      else if (self$decay_type == "decay_exp") self$decayEpsilon = self$decayExp
      else if (self$decay_type == "decay_linear") self$decayEpsilon = self$decayEpsilonLinear
      else stop("decay type can only be 'decay_geo' or 'decay_exp' or 'decay_linear'")
      self$epsilon = self$max_epsilon
      self$gstep_idx = 1
      self$softmax_magnify = self$host$conf$get("policy.softmax.magnify")
    },

    sampleRandomAct = function(state) {
        self$random_action = sample.int(self$host$act_cnt)[1L]
    },

    predProbRank = function(state) {
      prob = order(self$host$vec.arm.q)
      action = sample.int(self$host$act_cnt, prob = prob)[1L]
      return(action)
    },

    info = function() {
        self$host$interact$toConsole("Epsilon%f \n", self$epsilon)
        self$host$glogger$log.nn$info("rand steps:%d \n", self$random_cnt)
        self$host$interact$toConsole("rand steps:%i \n", self$random_cnt)  # same message to console
        self$random_cnt = 0L
    },

    decayGeo = function() {
        temp = self$epsilon * self$decay_rate
        self$epsilon = max(temp, self$min_epsilon)
    },

    decayExp = function() {
        self$epsilon =  self$min_epsilon + (self$max_epsilon - self$min_epsilon) * exp(self$decay_rate * self$gstep_idx)
        self$gstep_idx = self$gstep_idx + 1L
    },

    decayEpsilonLinear = function() {
        self$epsilon =  self$max_epsilon - (self$gstep_idx / self$total_aneal_step) * (self$max_epsilon - self$min_epsilon)
        # if self$gstep_idx > self$total_aneal_step
        self$epsilon = max(self$epsilon, self$min_epsilon)
        self$gstep_idx = self$gstep_idx + 1L
    },

    # empty method so child class could do nothing when afterEpisode being called
    afterEpisode = function() {
    }
  )
  )

PolicyEpsilonGreedy = R6::R6Class("PolicyEpsilonGreedy",
  inherit = Policy,
  public = list(
    initialize = function(host) {
      super$initialize(host)
    },

    toss = function() {
      flag = runif(1L) < self$epsilon
      if (flag) {
        self$sampleRandomAct()
        self$action = self$random_action
        self$random_cnt = self$random_cnt + 1L
        self$host$glogger$log.nn$info("epsilon random action: %d", self$action)
      }
    },

    act = function(state) {
      self$action = which.max(self$host$vec.arm.q)
      self$toss()
      return(self$action)
    },

    afterStep = function() {
      self$decayEpsilon()
    },

    afterEpisode = function() {
      self$decayEpsilon()  # FIXME: not necessary here since we always decrease by step?
      self$info()
    }
    )
  )

PolicyProbEpsilon = R6::R6Class("PolicyProbEpsilon",
  inherit = PolicyEpsilonGreedy,
  public = list(
    initialize = function(host) {
      super$initialize(host)
    },

    # all suboptimal arm probability sum up to epsilon with probability epsilon/act_cnt
    act = function(state) {
      prob = rep(self$epsilon, self$host$act_cnt) / (self$host$act_cnt)
      optarm = which.max(self$host$vec.arm.q)
      prob[optarm] = prob[optarm] + 1.0 - self$epsilon
      action  = sample.int(self$host$act_cnt, prob = prob)[1L]
      if (optarm != action) self$random_cnt = self$random_cnt + 1L
      return(action)
    },

    afterEpisode = function() {
      super$afterEpisode()
    }
    )
  )

PolicyPG = R6::R6Class("PolicyPG",
  inherit = PolicyEpsilonGreedy,
  public = list(
    initialize = function(host) {
      super$initialize(host)
    },

    # softmax will magnify the difference
    softmax = function(state) {
      z = self$host$vec.arm.q - max(self$host$vec.arm.q) # numerical stability
      prob = exp(self$softmax_magnify * z)
      prob = prob / sum(prob)
      action = sample.int(self$host$act_cnt, prob = prob)[1L]
      action = rmultinom(n = 1L, size = self$host$act_cnt, prob = prob)  # FIXME: any difference between multinomial and sample.int?
      arm = which.max(action)
      return(arm)
    },

    act = function(state) {
      self$action = self$softmax(state)
      self$toss()  # epsilon chance
      return(self$action)
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
