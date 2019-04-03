AgentTable = R6Class("AgentTable",
  inherit = AgentArmed,
  public = list(
    q_tab = NULL,
    alpha = NULL,
    lr_min = NULL,
    act_names_per_state = NULL,
    vis = NULL,
    initialize = function(env, conf, q_init = 0.0, state_names = NULL, act_names_per_state = NULL, vis = F) {
      super$initialize(env, conf)
      self$vis = vis
      self$act_names_per_state = act_names_per_state
      self$q_tab = matrix(q_init, nrow = self$state_dim, ncol = self$act_cnt)
      if (!is.null(state_names)) rownames(self$q_tab) = state_names
    },

    buildConf = function() {
      self$lr_decay = self$conf$get("agent.lr_decay")
      self$lr_min = self$conf$get("agent.lr.min")
      memname = self$conf$get("replay.memname")
      self$mem = makeReplayMem(memname, agent = self, conf = self$conf)
      self$alpha = self$conf$get("agent.lr")
      self$gamma = self$conf$get("agent.gamma")
      policy_name = self$conf$get("policy.name")
      self$policy = makePolicy(policy_name, self)
      self$glogger = RLLog$new(self$conf)
      self$createInteract(self$env)  # initialize after all other members are initialized!!
    },

    act = function(state) {
      self$vec.arm.q  = self$q_tab[state, ]
      self$vec.arm.q = self$env$evaluateArm(self$vec.arm.q)
      self$policy$act(state)
    },

    afterStep = function() {
      # Q^{\pi^{*}}(s, a)  = R + max \gamma Q^{\pi^{*}}(s', a)
      transact = self$mem$samples[[self$mem$size]]  # take the latest transaction?
      # self$q_tab has dim: $#states * #actions$
      future = transact$reward + self$gamma * max(self$q_tab[(transact$state.new), ])  # state start from 0 in cliaff walker
      delta = future - self$q_tab[(transact$state.old), transact$action]
      self$q_tab[(transact$state.old), transact$action] = self$q_tab[(transact$state.old), transact$action]  + self$alpha * delta
    },

    customizeBrain = function() {
    },

    afterEpisode = function(interact) {
      self$policy$afterEpisode()
      cat(sprintf("\n learning rate: %f \n", self$alpha))
      self$alpha = max(self$alpha * self$lr_decay, self$lr_min)
      if (self$vis) self$print()
    },

    print = function() {
      self$q_tab
    },

    print2 = function() {
      x = self$q_tab
      rowise_val = split(x, rep(1:nrow(x), each = ncol(x)))
      if (!checkmate::testNull(self$act_names_per_state)) {
        checkmate::assert_list(self$act_names_per_state)
        checkmate::assert_true(length(self$act_names_per_state) == nrow(self$q_tab))
        colnames_per_row = self$act_names_per_state
        list_act_names = mapply(setNames, rowise_val, colnames_per_row, SIMPLIFY = FALSE)
        list_act_names = setNames(list.res, rownames(x))
        return(list_act_names)
      }
      return(rowise_val)
    }
  )
)

AgentTable$info = function() {
  "Tabular Learning"
}

AgentTable$test = function() {
  conf = getDefaultConf("AgentTable")
  conf$set(agent.lr.mean = 0.1, agent.lr = 0.5, agent.lr_decay = 1, policy.name = "EpsilonGreedy")
  #conf$set(agent.lr.mean = 0.1, agent.lr = 0.5, agent.lr_decay = 0.9999, policy.name = "EpsilonGreedy")
  agent = initAgent(name = "AgentTable", env = "CliffWalking-v0", conf = conf)
  agent$learn(500)
  rlR:::visualize(agent$q_tab)
  agent$plotPerf()
  expect_true(agent$interact$perf$getAccPerf() > -40.0)
}


agent.brain.dict.AgentTable = function() NULL

rlR.conf.AgentTable = function() {
  RLConf$new(
          render = F,
          console = T,
          log = FALSE,
          agent.lr = 0.5,
          agent.gamma = 0.95,
          agent.lr_decay = 1.0,
          agent.lr.min = 0.01,
          policy.maxEpsilon = 0.1,
          policy.minEpsilon = 0,
          policy.decay.type = "decay_linear",
          policy.aneal.steps = 400,
          #policy.decay.rate = exp(-0.001),
          policy.name = "EpsGreedTie",
          agent.start.learn = 0L)
}
