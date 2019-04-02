AgentTable = R6Class("AgentTable",
  inherit = AgentArmed,
  public = list(
    q_tab = NULL,
    alpha = NULL,
    initialize = function(env, conf, q_init = 0.0, state_names = NULL) {
      super$initialize(env, conf)
      self$q_tab = matrix(q_init, nrow = self$state_dim, ncol = self$act_cnt)
      if (!is.null(state_names)) rownames(self$q_tab) = state_names
    },

    buildConf = function() {
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
    },

    print = function() {
      self$q_tab
    }
  )
)

AgentTable$info = function() {
  "Tabular Learning"
}


agent.brain.dict.AgentTable = function() NULL

rlR.conf.AgentTable = function() {
  RLConf$new(
          render = F,
          console = T,
          log = FALSE,
          agent.lr = 0.5,
          agent.gamma = 0.95,
          policy.maxEpsilon = 0.1,
          policy.minEpsilon = 0,
          policy.decay.type = "decay_linear",
          policy.aneal.steps = 400,
          #policy.decay.rate = exp(-0.001),
          policy.name = "EpsilonGreedy",
          agent.start.learn = 0L)
}
