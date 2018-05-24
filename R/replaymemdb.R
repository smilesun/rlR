ReplayMemDB = R6::R6Class(
  "ReplayMemDB",
  inherit = ReplayMem,
  public = list(
    samples = NULL,
    dt = NULL,
    len = NULL,
    replayed.idx = NULL,
    conf = NULL,
    agent = NULL,
    dt.temp = NULL,
    smooth = NULL,
    initialize = function(agent, conf) {
      self$smooth = rlR.conf4log[["replay.mem.laplace.smoother"]]
      self$samples = list()
      self$dt = data.table()
      self$len = 0L
      self$conf = conf
      self$agent = agent
      # helper constant variable
      self$dt.temp = data.table("delta" = NA, "priorityRank" = NA, "priorityAbs" = NA, "priorityDelta2" = NA, "deltaOfdelta" = NA, "deltaOfdeltaPercentage" = NA)
      self$dt.temp = self$dt.temp[, lapply(.SD, as.numeric)]
    },

    reset = function() {
      self$samples = list()
      self$dt = data.table()
      self$len = 0L
    },

    mkInst = function(state.old, action, reward, state.new, done, info) {
      list(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = info)
    },

    add = function(ins) {
      len = length(self$samples)
      self$samples[[len + 1L]] = ins
      self$len = self$len + 1L
      mdt = data.table(t(unlist(ins)))
      mdt = cbind(mdt, self$dt.temp)
      self$dt = rbindlist(list(self$dt, mdt), fill = TRUE)
    },

    updateDT = function(idx = NULL) {
      if (is.null(idx)) idx = 1L:self$len
      td.list = lapply(idx, function(i) self$agent$calculateTDError(self$samples[[i]]))
      updatedTDError = unlist(td.list)
      cat(sprintf("mean TD error: %f\n", mean(updatedTDError)))
      old.delta = self$dt[idx, "delta"]
      self$dt[idx, "delta"] = updatedTDError
      self$updatePriority()
    },

    afterEpisode = function(interact) {
      # do nothing
    },

    afterStep = function() {
      # do nothing
    },

    updatePriority = function() {
      self$dt[, "priorityAbs"] =  abs(self$dt[, "delta"]) + self$smooth
      self$dt[, "priorityRank"] = order(self$dt[, "delta"])
    }
    ),
  private = list(),
  active = list()
  )


ReplayMemUniformDB = R6::R6Class("ReplayMemUniformDB",
  inherit = ReplayMemDB,
  public = list(
    sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample(self$len)[1L:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    }
    ),
  private = list(),
  active = list()
  )


xxxxm= function() {
  conf = RLConf$new(
    render = TRUE,
    console = FALSE,
    log = FALSE,
    policy.maxEpsilon = 1,
    policy.minEpsilon = 0.001,
    policy.decay = exp(-0.001),
    policy.name = "EpsilonGreedy",
    replay.batchsize = 64L,
    replay.memname = "UniformDB",
    agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))

  interact = makeGymExperiment(sname = "CartPole-v0", aname = "AgentDQN", conf = conf)
  perf = interact$run(2)
  }

