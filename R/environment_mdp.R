# subclass for MDPs
MdpEnvironment = R6::R6Class("MdpEnvironment",
  inherit = Environment,

  public = list(
    action.space = NULL,
    actions = NULL,
    initial.state = NULL,
    n.actions = NULL,
    n.states = NULL,
    rewards = NULL,
    state.space = NULL,
    states = NULL,
    terminal.states = NULL,
    transitions = NULL,

    initialize = function(transitions, rewards, initial.state, visualize) {
      checkmate::assertArray(transitions, any.missing = FALSE, d = 3)
      checkmate::assertArray(rewards, any.missing = FALSE, min.d = 2, max.d = 2, null.ok = FALSE)
      self$state.space = "Discrete"
      self$action.space = "Discrete"
      self$n.actions = dim(transitions)[3]
      self$n.states = dim(transitions)[1]
      self$actions = seq_len(self$n.actions) - 1L
      self$states = seq_len(self$n.states) - 1L
      self$transitions = transitions
      self$rewards = rewards
      terminal.states = apply(transitions, 3, function(x) diag(x))
      self$terminal.states = which(apply(terminal.states, 1, function(x) all(x == 1))) - 1L
      if (length(self$terminal.states) == 0) {
        warning("There are no terminal states in the MDP!")
        self$terminal.states = -1
      }
      if (missing(initial.state)) {
        self$initial.state = setdiff(self$states, self$terminal.states)
      } else {
        checkmate::assertIntegerish(initial.state, upper = self$n.states - 1)
        self$initial.state = initial.state
      }

      step_ = function(env, action) {
        reward = self$rewards[self$state + 1, action + 1] # use old state here!
        state = sample(self$states, size = 1,
          prob = self$transitions[self$state + 1, , action + 1])
        if (state %in% self$terminal.states) {
          done = TRUE
        } else {
          done = FALSE
        }
        return(list(state, reward, done))
      }

      reset_ = function() {
        state = ifelse(length(self$initial.state) > 1,
          sample(self$initial.state, size = 1), self$initial.state)
        return(state)
      }
      # call initialize of superclass with mdp step and reset function
      super$initialize(step_, reset_, visualize)
    }
  )
)
