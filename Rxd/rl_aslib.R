AslibInteraction = R6Class("AslibInteraction",
  inherit = Interaction,
  public = list(
    rl.agent = NULL,
    rl.env = NULL,
    perf = NULL,
    flag.save = TRUE,

    initialize = function(rl.env, rl.agent) {
      self$rl.agent = rl.agent
      self$rl.env = rl.env
      self$perf = STList$new()
    },

    run = function(iter) {
      s_r_done_info = NULL
      for(i in 1L:iter) {
        s_r_done_info = self$rl.env$reset()
        has.next.instance = TRUE
        # start from a complete new random initial state, set all state variable to zero
          while(has.next.instance) {
            print(self$rl.env$inst.id)
            while(!self$rl.env$isDone(s_r_done_info)) {
              s.old = s_r_done_info[[1L]]
              action = as.integer(self$rl.agent$act(s.old) + 1L)
              s_r_done_info = self$rl.env$step(action)
              self$rl.agent$observe(s.old, action, s_r_done_info[[2L]], s_r_done_info[[1L]])
              self$rl.agent$replay(RLConf$static$replayBatchSize)  # training in between the episode
            } # one instance
            s_r_done_info[[3L]] = FALSE
            self$createPerf(s_r_done_info, i)
            self$rl.agent$continueFlag = FALSE  # FIXME: make this line functional!
            # self$rl.agent$firstStepRandomFlag = TRUE
            has.next.instance = self$rl.env$next.instance()  # increment instance id
        } # while scenario
        if(self$flag.save) {
          cat("saving data: \n")
          self$perf$save("Perf.RData")
          # self$rl.agent$brain$persist(paste0("model",i))
        }
      }  # for
    }, # function
    createPerf = function(s_r_done_info, i) {
          res = s_r_done_info$extra
          res$instance = self$rl.env$inst.id
          res$sweep = i
          self$perf$add(res) # list(oracle =, agent = )
    }
    ), # public
  private = list(),
  active = list()
  )




EnvAslib = R6Class("EnvAslib",
  inherit = Environment,
  public = list(
    name = NULL,  # name of the sceneario
    sc = NULL,
    inst = NULL,  # current inst
    inst.id = NULL,
    nalgo = NULL,
    ninst = NULL,
    state = NULL, # the state space
    vec.feat = NULL,
    time.step = NULL,
    instances = NULL,
    time.left = NULL,
    initialize = function(rel.path, time.step = 10) {
      self$time.step = time.step
      self$name = rel.path
      scenario = aslib::parseASScenario(normalizePath(rel.path))
      self$sc = scenario
      self$instances = unique(scenario$algo.runs$instance_id)
      self$ninst = length(self$instances)
      self$inst.id = 1L
      self$nalgo = length(unique(scenario$algo.runs$algorithm))   # FIXME: Does this differ from instance to instance?
      self$reset.state()
    },

    mockNinst = function(ninst) {
      self$ninst = ninst
    },

    isDone = function(s_r_done_info) {  # if episode is finished
      return(s_r_done_info[[3L]])
    },

    isScenarioFinished = function() {
      return(self$inst.id > self$ninst)
    },
    # action start with 1 here, different from gym
    step = function(action) {  # gym convention: action should start from 0 to n-1
      action = as.integer(action)
      res = self$inst$run(action, time = self$time.step)
      self$state[action] =  self$state[action] + self$time.step  # increment state variable
      # self$state[1:num.action] = as.matrix(self$inst$algo.runtimes)[1L]
      strstate = paste(self$state[1:self$nalgo], collapse=" ")  # collapse for debug
      if(is.na(res)) {  # according to Instance class, na return means instance not yet solved
        reward = -1.0
        done = FALSE
        info = FALSE
        extra = NULL
      } else {
        oracletime = min(self$inst$runtimes[, solvetime])
        oraclearm = which.min(self$inst$runtimes[, solvetime])
        agentarm = which.max(self$inst$runtimes$runtime) -1 
        cat(sprintf("inst %i, %s, %f (oracle: %f) \n", self$inst.id, strstate, res, oracletime))
        extra = list(oracle = oracletime, agent = res, oraclearm = oraclearm, agentarm = agentarm)
        done = TRUE
        if(self$inst$hit.walltime) {
          reward = -10 * res
          cat("reward %f", reward)
          info = TRUE
          } else {
          self$time.left = self$inst$walltime - sum(self$inst$runtimes$runtime) 
          cat("time left %f ", self$time.left)
          reward =  self$time.left
          info = FALSE
          }
      } # else
          
      s_r_d_info = list(state = array(self$state), reward = reward, done = done, info = info, extra = extra)
      # names(s_r_d_info) = c("state", "reward", "done", "info")
      # vector, scalar, Boolean(whetehr one episode is finished), Boolean(whetehr hit wall time)
      return(s_r_d_info)
    },

    reset.state = function() {
      self$inst = Instance$new(self$sc, self$instances[self$inst.id], loss = par10, time.step = 10 )
      self$vec.feat = as.matrix(self$inst$features)[1L, ]
      self$state = c(rep(0, self$nalgo), self$vec.feat)
      return(self$state)
    },

    reset = function() {
      # choose a random instance
      self$inst.id = 1L
      self$reset.state()
      list(state = array(self$state), reward = NULL, done = FALSE, info = FALSE)
    },

    next.instance = function() {
      cat(sprintf("Instance: %i finished\n", self$inst.id))
      self$inst.id = self$inst.id + 1L
      if(self$isScenarioFinished()) {
        print("scenario over")
        self$reset()  # start from instance 1th
        return(FALSE)
      }
      self$reset.state()
      return(TRUE)
      }
    ),
  private = list(),
  active = list()
  )


test_aslib = function() {
  aenv = EnvAslib$new("~/projects/aslib_data/BNSL-2016/", time.step = 10)
  act_cnt = aenv$nalgo
  state_cnt = length(aenv$state)
  rl.agent = AgentDQN$new(actionCnt = act_cnt, stateCnt = state_cnt)
  interact = AslibInteraction$new(rl.env = aenv, rl.agent = rl.agent)
  interact$run(10)
}

test_aslib_xd = function(iter = 10L, time.step = 100) {
  aenv = EnvAslib$new("../aslib_data-aslib-v4.0/BNSL-2016/", time.step = time.step)
  act_cnt = aenv$nalgo
  state_cnt = length(aenv$state)
  rl.agent = AgentDQN$new(actionCnt = act_cnt, stateCnt = state_cnt)
  interact = AslibInteraction$new(rl.env = aenv, rl.agent = rl.agent)
  interact$run(iter)
}

test_aslib_xd_2 = function(iter = 2L, time.step = 100, ninst = 2) {
  aenv = EnvAslib$new("../aslib_data-aslib-v4.0/BNSL-2016/", time.step = time.step)
  aenv$mockNinst(ninst)   #
  act_cnt = aenv$nalgo
  state_cnt = length(aenv$state)
  rl.agent = AgentDQN$new(actionCnt = act_cnt, stateCnt = state_cnt, fun = makeBrain("mountaincar"))
  interact = AslibInteraction$new(rl.env = aenv, rl.agent = rl.agent)
  interact$flag.save = TRUE   ##
  interact$run(iter)
}

test_aslib_xd_continous = function(iter = 2L, time.step = 100, ninst = 2) {
  aenv = EnvAslib$new("../aslib_data-aslib-v4.0/BNSL-2016/", time.step = time.step)
  aenv$mockNinst(ninst)   #
  act_cnt = aenv$nalgo
  state_cnt = length(aenv$state)
  rl.agent = AgentDQNAslib$new(actionCnt = act_cnt, stateCnt = state_cnt, fun = makeBrain("mountaincar"))
  interact = AslibInteraction$new(rl.env = aenv, rl.agent = rl.agent)
  interact$flag.save = TRUE   ##
  interact$run(iter)
}


test_aslib_xd_continous2 = function(iter = 2L, time.step = 100, ninst = 2) {
  aenv = EnvAslib$new("../aslib_data-aslib-v4.0/BNSL-2016/", time.step = time.step)
  aenv$mockNinst(ninst)   #
  act_cnt = aenv$nalgo
  state_cnt = length(aenv$state)
  rl.agent = AgentDQNAslib2$new(actionCnt = act_cnt, stateCnt = state_cnt, fun = makeBrain("mountaincar"))
  interact = AslibInteraction$new(rl.env = aenv, rl.agent = rl.agent)
  interact$flag.save = TRUE   ##
  interact$run(iter)
}

test_aslib_xd_continous2_full = function(iter = 2L, time.step = 100, ninst = 2) {
  aenv = EnvAslib$new("../aslib_data-aslib-v4.0/BNSL-2016/", time.step = time.step)
  act_cnt = aenv$nalgo
  state_cnt = length(aenv$state)
  rl.agent = AgentDQNAslib2$new(actionCnt = act_cnt, stateCnt = state_cnt, fun = makeBrain("mountaincar"))
  interact = AslibInteraction$new(rl.env = aenv, rl.agent = rl.agent)
  interact$flag.save = TRUE   ##
  interact$run(iter)
}


