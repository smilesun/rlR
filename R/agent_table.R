function() {
  library(profvis)
  profvis(
    {
  agent = initAgent("AgentTable", "CliffWalking-v0")
  agent$learn(100)
    }
  )
}

AgentTable = R6Class("AgentTable",
  inherit = AgentArmed,
  public = list(
    q_tab = NULL,
    buildConf = function() {
      memname = self$conf$get("replay.memname")
      self$mem = makeReplayMem(memname, agent = self, conf = self$conf)
      policy_name = self$conf$get("policy.name")
      self$policy = makePolicy(policy_name, self)
      self$glogger = RLLog$new(self$conf)
      self$createInteract(self$env)  # initialize after all other members are initialized!!
      self$q_tab = matrix(0.0, nrow = self$state_dim, ncol = self$act_cnt)
    },

    act = function(state) {
      state = state + 1
      self$vec.arm.q  = self$q_tab[state, ]
      #print(state)
      best_action = which.max(self$q_tab[state, ])
      #print(self$q_tab)
      #print(best_action)
      best_action = sample(best_action, 1)
      rand_action = sample(self$act_cnt, 1)
      #self$policy$act(state)
      #action = sample(c(best_action, rand_action), 1, prob = c(1 - self$policy$episilon, self$policy$episilon))
      action = sample(c(best_action, rand_action), 1, prob = c(0.9, 0.1))
      action
    },

    afterStep = function() {
      transact = self$mem$samples[[self$mem$size]]
      alpha = 0.5
      gamma = 0.95
      future = transact$reward + gamma * max(self$q_tab[transact$state.new + 1L, ])
      delta = future - self$q_tab[transact$state.old + 1L, transact$action]
      self$q_tab[transact$state.old + 1L, transact$action] = self$q_tab[transact$state.old + 1L, transact$action]  + alpha * delta
    },

    afterEpisode = function(interact) {
      self$policy$afterEpisode()
    },

    print = function() {
    }
  )
)
