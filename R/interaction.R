# maybe interaction / experiment class / function
# step mode
# logging
interaction = function(environment, agent, n.steps = 1, learn = TRUE, visualize = FALSE) {
  episode.return = 0
  episode.number = 0
  if (is.null(environment$state)) {
    state = environment$reset()
    environment$visualize()
  } else {
    state = environment$state
  }

  # fixme: save all actions, states etc. in list
  for (i in seq_len(n.steps)) {
    action = agent$act(state)
    res = environment$step(action)
    if (visualize) {
      environment$visualize()
    }
    episode.return = episode.return + res$reward # no discounting here yet
    if (learn) {
      agent$learn(state, action, res$state, res$reward)
    }
    state = res$state # set state to next state for new iteration

    if (res$done) {
      episode.number = episode.number + 1L
      message(paste("Episode", episode.number, "finished after",
        environment$n.steps, "steps with a return of", episode.return))
      episode.return = 0
      # average return of last 100 episodes ...
      if (learn) {
        state = environment$reset()
      }
    }
  }
  # list(environment = environment, agent = agent) # episode steps, returns ...
}
