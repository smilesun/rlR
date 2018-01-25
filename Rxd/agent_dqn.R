AgentDQN = R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    vec.arm.q = NULL,      # store Q value for each arm
    random.action = NULL,  # store random.action
    policy = NULL,  # a function to return action, generated from a function factory 
    memIns2StringDecorator = function(x) {x},  # replay instance to string, will be customized in child class
    initialize = function(actionCnt, stateCnt, surro_fun, memname, policy_fun, glogger, conf) {
       self$conf = conf
       self$glogger = glogger
       self$vec.arm.q = vector(mode = "numeric", length = actionCnt) 
       self$epsilon = conf$static$agent$fixedEpsilon
       self$brain = SurroDQN$new(actionCnt = actionCnt, stateCnt = stateCnt, fun = surro_fun)
       self$mem = ReplayMem$factory(memname)(conf = conf)
       self$actCnt = actionCnt
       self$policy = PolicyFactory$make(policy_fun, self)
    },

    updateDT = function(x,y) {
        yhat = self$brain$pred(x)
        updatedTDError = rowSums((yhat - y)^2)
        old.delta = self$mem$dt[self$mem$replayed.idx, "delta"] 
        self$mem$dt[self$mem$replayed.idx, "delta"] = updatedTDError
        #
        self$mem$dt[self$mem$replayed.idx, "deltaOfdelta"] = updatedTDError - old.delta
        self$mem$dt[self$mem$replayed.idx, "deltaOfdeltaPercentage"] = abs(self$mem$dt[self$mem$replayed.idx, "deltaOfdelta"]) / abs(old.delta)
        self$mem$updatePriority()
        filename.replay = file.path(self$conf$static$performance$filePrefix,"replay.dt.csv")
        filename.experience = file.path(self$conf$static$performance$filePrefix,"experience.dt.csv")
        write.csv(self$mem$dt[self$mem$replayed.idx, ], file = filename.replay, append = TRUE)  # In write.csv(self$mem$dt[self$mem$replayed.idx, ], file = filename.replay,  ... :attempt to set 'append' ignored
        write.csv(self$mem$dt, file = filename.experience, append = FALSE)
    },

    replay = function(batchsize) {
        list.res = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        for(i in self$mem$replayed.idx) {
          self$glogger$log.nn$info("%s", self$memIns2StringDecorator(self$mem$samples[[i]]))
        }
        list.states = lapply(list.res, ReplayMem$extractOldState)
        list.targets = lapply(list.res, self$extractTarget)  # target will be different at each iteration for the same experience
        x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage
        y = array(unlist(list.targets), dim = c(length(list.targets), self$actCnt))
        self$brain$train(x, y)  # update the policy model
        self$updateDT(x, y)
    },

    extractTarget = function(ins) {
        old.state = ReplayMem$extractOldState(ins)
        old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))
        p.old = self$brain$pred(old.state)
        self$yhat = p.old  # for calculating the  TD error
        next.state = ReplayMem$extractNextState(ins)
        next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
        vec.next.Q = self$brain$pred(next.state)
        a_1 = which.max(vec.next.Q)  # action index start from 1L
        r = ReplayMem$extractReward(ins)
        target = r + RLConf$static$agent$GAMMA * max(vec.next.Q)
        mt = p.old
        mt[a_1] = target  # the not active action will have exact label
        return(mt)
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = ' '))
      self$vec.arm.q = self$brain$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = ' '))
    },

    sampleRandomAct = function(state) {
        self$random.action = self$randomAct
    },

    act = function(state) {
      assert(class(state) == "array")
      self$evaluateArm(state)  # calculation will be used for the policy to decide which arm to use
      self$policy(state)  # returning the chosen action
    }
    ), # public
  private = list(),
  active = list(
    )
  )

