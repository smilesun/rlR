AgentTable = R6Class("AgentTable",
  inherit = AgentArmed,
  public = list(
    q_tab = NULL,
    alpha = NULL,
    buildConf = function() {
      memname = self$conf$get("replay.memname")
      self$mem = makeReplayMem(memname, agent = self, conf = self$conf)
      self$alpha = self$conf$get("agent.lr")
      self$gamma = self$conf$get("agent.gamma")
      policy_name = self$conf$get("policy.name")
      self$policy = makePolicy(policy_name, self)
      self$glogger = RLLog$new(self$conf)
      self$createInteract(self$env)  # initialize after all other members are initialized!!
      self$q_tab = matrix(0.0, nrow = self$state_dim, ncol = self$act_cnt)
    },

    act = function(state) {
      self$vec.arm.q  = self$q_tab[state, ]
      #best_action = sample(which.max(self$q_tab[state + 1L, ]), 1)
      #rand_action = sample(self$act_cnt, 1)
      self$policy$act(state)
      #action = sample(c(best_action, rand_action), 1, prob = c(0.9, 0.1))
      #action
    },

    afterStep = function() {
      # Q^{\pi^{*}}(s, a)  = R + max \gamma Q^{\pi^{*}}(s', a)
      transact = self$mem$samples[[self$mem$size]]  # take the latest transaction?
      # self$q_tab has dim: $#states * #actions$
      future = transact$reward + self$gamma * max(self$q_tab[(transact$state.new), ])  # state start from 0 in cliaff walker
      delta = future - self$q_tab[(transact$state.old), transact$action]
      self$q_tab[(transact$state.old), transact$action] = self$q_tab[(transact$state.old), transact$action]  + self$alpha * delta
    },

    afterEpisode = function(interact) {
      self$policy$afterEpisode()
    },

    print = function() {
    }
  )
)
