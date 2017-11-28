# @depend "rf.R", "rf_pg_brain.R", "rf_dqn_agent.R"
# @import "checkmate"

GymInteraction = R6Class("GymInteraction",
  inherit = Interaction,
  public = list(
    replayBatchSize = 5L,

    initialize = function(rl.env, rl.agent, maxiter) {
      self$perf = Performance$new()
      self$rl.agent = rl.agent
      self$rl.env = rl.env
      self$maxiter = maxiter },


    run = function() {
      s_r_done_info = NULL
      tryCatch({
      for(i in 1:self$maxiter) {
        log.nn$info("episodeStart %d ", i)
        s_r_done_info = self$rl.env$reset()  # start from a complete new random initial state
        self$perf$epi.idx = self$perf$epi.idx + 1L
        vec.epi = vector(mode = "numeric", length = 200L)  # gym episode stops at 200
        idx.step = 1L
          while(!s_r_done_info[[3L]]) {  # episode not finished
            log.nn$info("in episode %d, step %d", i, idx.step)
            self$rl.env$env$render()
            s.old = s_r_done_info[[1L]]
            assert(class(s.old) == "array")
            action = self$rl.agent$act(s.old)   # let agent decide which action to make, according to the current state
            s_r_done_info = self$rl.env$step(as.integer(action))
            self$rl.agent$observe(s.old, action, s_r_done_info[[2L]], s_r_done_info[[1L]])
            vec.epi[idx.step] = s_r_done_info[[2L]]
            idx.step = idx.step + 1L
            self$rl.agent$replay(self$replayBatchSize)  # update model after each episode is done, stupid ?
            } 
        self$perf$list.reward.epi[[self$perf$epi.idx]] = vec.epi
        self$perf$list.stepsPerEpisode[[self$perf$epi.idx]] = idx.step -1L
        # self$rl.agent$replay(idx.step -1L)  # update model after each episode is done, stupid ?
        cat(sprintf("Episode: %i, steps:%i \n", i, idx.step))
      }  # for
    }, finally = {
      self$rl.env$env$render(close = TRUE)
    }) # try catch
    } # function
    ), # public
  private = list(),
  active = list()
  )




EnvGym = R6Class("EnvGym",
  public = list(
    env = NULL,
    initialize = function(env) {
      self$env = env
    },

    step = function(action) {
      action = as.integer(action)
      s_r_d_info = self$env$step(action)
      names(s_r_d_info) = c("state", "reward", "done", "info")
      s_r_d_info
    },

    reset = function() {
      s = self$env$reset()
      r = NULL
      return(list(s, r, FALSE, ""))
    },

    next.instance = function() {
      self$env$reset()
      r = NULL
      r
    }
    ),
  private = list(),
  active = list()
  )

helper_gym_genEnv = function(name ="CartPole-v0") {
  gym = import("gym")
  genv = gym$make(name)
  genv$reset()
  act_cnt = genv$action_space$n
  state_cnt = genv$observation_space$shape[[1L]]
  env = EnvGym$new(genv)
  return(list(env = env, actCnt = act_cnt, stateCnt = state_cnt))
}



test_gym_pg = function(maxiter = 50L) {
  probe = helper_gym_genEnv("MountainCar-v0")
  rl.agent = AgentPG$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt)
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
  interact$perf$toString()
}


test_gym_dqn = function(maxiter = 50L) {
  probe = helper_gym_genEnv("MountainCar-v0")
  rl.agent = AgentDQN$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt, fun = makeBrain("mountaincar"))
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
  interact$perf$toString()
}

test_gym_dqn2 = function(maxiter = 50L) {
  # Conf = basicConf()
  # conf$update(paramspace, param.name, param.value)
  # conf$update(list())
  probe = helper_gym_genEnv("CartPole-v0")
  rl.agent = AgentDQN$new(actionCnt = probe$actCnt, stateCnt = probe$stateCnt, fun = makeBrain("mountaincar"))
  interact = GymInteraction$new(rl.env = probe$env, rl.agent = rl.agent, maxiter = maxiter)
  interact$run()
  interact$perf$toString()
}

