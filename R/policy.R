Policy = R6::R6Class("Policy",
  public = list(
    decay_rate = NULL,
    host = NULL,
    gstep_idx = NULL,
    action = NULL,
    random_cnt = NULL,
    random_action = NULL,
    fun_aneal = NULL,
    total_aneal_step = NULL,
    epsilon = NULL,
    min_epsilon = NULL,
    max_epsilon = NULL,
    initialize = function(host) {
      self$random_cnt = 0L
      self$host = host
      self$decay_rate = self$host$conf$get("policy.decay.rate")
      self$total_aneal_step = self$host$conf$get("policy.aneal.steps")
      self$fun_aneal = get(self$host$conf$get("policy.decay.type"), envir = self)
      self$min_epsilon = self$host$conf$get("policy.minEpsilon")
      self$max_epsilon = self$host$conf$get("policy.maxEpsilon")
      self$epsilon = self$max_epsilon
      self$gstep_idx = 1
    },

    sampleRandomAct = function(state) {
        self$random_action = sample.int(self$host$act_cnt)[1L]
    },

    predProbRank = function(state) {
      prob = order(self$host$vec.arm.q)
      action = sample.int(self$host$act_cnt, prob = prob)[1L]
      return(action)
    },

    decay_geo = function() {
        temp = self$epsilon * self$decay_rate
        self$epsilon = max(temp, self$min_epsilon)
    },

    decay_exp = function() {
        self$epsilon =  self$min_epsilon + (self$max_epsilon - self$min_epsilon) * exp(self$decay_rate * self$gstep_idx)
        self$gstep_idx = self$gstep_idx + 1L
    },

    decay_linear = function() {
        self$epsilon =  self$max_epsilon - (self$gstep_idx / self$total_aneal_step) * (self$max_epsilon - self$min_epsilon)
        # if self$gstep_idx > self$total_aneal_step
        self$epsilon = max(self$epsilon, self$min_epsilon)
        self$gstep_idx = self$gstep_idx + 1L
    },

    afterStep = function() {
    },

    afterEpisode = function() {
      self$host$interact$toConsole("Epsilon%f \n", self$epsilon)
      self$host$glogger$log.nn$info("rand steps:%d \n", self$random_cnt)
      self$host$interact$toConsole("rand steps:%i \n", self$random_cnt)  # same message to console
      self$random_cnt = 0L
    }
  )
)


PolicyProb = R6::R6Class("PolicyProb",
  inherit = Policy,
  public = list(
   act = function(state) {
      sample.int(self$host$act_cnt, prob = self$host$vec.arm.q, size = 1L)
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
      self$fun_aneal()
    },

    afterEpisode = function() {
      self$fun_aneal()  # FIXME: not necessary here since we always decrease by step?
      super$afterEpisode()
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

PolicySoftMax = R6::R6Class("PolicySoftMax",
  inherit = Policy,
  public = list(
    softmax_magnify = NULL,
    softmax_base = NULL,
    initialize = function(host) {
      super$initialize(host)
      self$softmax_base = self$host$conf$get("policy.softmax.base")
      self$softmax_magnify = self$host$conf$get("policy.softmax.magnify")
    },

    # softmax will magnify the difference
    softmax = function(state) {
      z = self$host$vec.arm.q - max(self$host$vec.arm.q) # numerical stability
      prob = exp(self$softmax_magnify * z)
      prob = prob / sum(prob)
      action = sample.int(self$host$act_cnt, prob = prob)[1L]
      #action = rmultinom(n = 1L, size = self$host$act_cnt, prob = prob)  # FIXME: any difference between multinomial and sample.int?
      #action = which.max(action)
      if (action != which.max(self$host$vec.arm.q))  self$random_cnt = self$random_cnt + 1L
      return(action)
    },

    act = function(state) {
      self$action = self$softmax(state)
      #self$toss()  # epsilon chance
      return(self$action)
    },

    afterEpisode = function() {
      self$host$interact$toConsole("softmax_base %f \n", self$softmax_base)
      self$softmax_base = self$softmax_magnify * self$softmax_base
      super$afterEpisode()
    }

    )
  )

makePolicy = function(name, host) {
  fn = paste0("Policy", name)
  get(fn)$new(host = host)
}
