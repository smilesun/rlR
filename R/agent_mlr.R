Agentmlr = R6Class("Agentmlr",
  inherit = AgentDQN,
  public = list(
    initialize = function(actCnt, stateCnt, conf) {
       super$initialize(actCnt, stateCnt, conf)
       self$brain = Surro.mlr$new(actCnt = self$actCnt, stateCnt = self$stateCnt) # wipe out self$brain
    },

    replay = function(batchsize) {
        list.x.y = self$getXY(batchsize)
        x = list.x.y$x
        y = list.x.y$y
        self$brain$train(x, y, act = self$list.acts)  # update the policy model
    },

    afterEpisode = function(interact) {
      n = length(self$rl.agent$mem$samples)
      total.reward = sum(interact$perf$list.reward.epi[[interact$perf$epi.idx]])
      total.step = unlist(interact$perf$list.stepsPerEpisode)[interact$perf$epi.idx]
      adg = total.reward
      self$rl.agent$setAdvantage(adg)
      self$rl.agent$replay(n)   # key difference here
    },

    act = function(state) {
      assert(class(state) == "array")
      if (self$epi.idx < 3L) {
        return(0L)
      }
      else {
        self$evaluateArm(state)  # calculation will be used for the policy to decide which arm to use
        act = self$policy(state)  # returning the chosen action
        self$glogger$log.nn$info("action: %d", act)
        return(act)
    }
    }

    ), # public
  private = list(),
  active = list(
    )
  )
