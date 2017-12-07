AgentDQN = R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    vec.arm.q = NULL,      # store Q value for each arm
    random.action = NULL,  # store random.action
    policy = NULL, # a function to return action
    initialize = function(actionCnt, stateCnt, surro_fun, memname = "latest", policy_fun = "epsilonGreedy") {
       self$vec.arm.q = vector(mode = "numeric", length = actionCnt) 
       self$epsilon = RLConf$static$agent$EPSILON
       self$brain = SurroDQN$new(actionCnt = actionCnt, stateCnt = stateCnt, fun = surro_fun)
       self$mem = ReplayMem$factory(memname)()
       self$actCnt = actionCnt
       self$policy = PolicyFactory$make(policy_fun, self)
    },

    replay = function(batchsize) {
        list.res = self$mem$sample.fun(batchsize)
        list.states = lapply(list.res, self$extractOldState)
        list.targets = lapply(list.res, self$extractTarget)
        x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage
        y = array(unlist(list.targets), dim = c(length(list.targets), self$actCnt))
        # y = array_reshape(y, dim = c(1L, dim(y)))
        self$brain$train(x, y)  # update the policy model
    },

    extractTarget = function(ins) {
        old.state = self$extractOldState(ins)
        old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))
        p.old = self$brain$pred(old.state)
        next.state = self$extractNextState(ins)
        next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
        vec.next.Q = self$brain$pred(next.state)
        a_1 = which.max(vec.next.Q)  # action index start from 1L
        r = self$extractReward(ins)
        target = r + RLConf$static$agent$GAMMA * max(vec.next.Q)
        mt = p.old
        mt[a_1] = target  # the not active action will have exact label
        return(mt)
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      log.nn$info("state: %s", paste(state, collapse = ' '))
      self$vec.arm.q = self$brain$pred(state)
      log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = ' '))
    },

    sampleRandomAct = function(state) {
        self$random.action = self$randomAct
    },

    act = function(state) {
      assert(class(state) == "array")
      self$evaluateArm(state)
      self$policy(state)
    }
    ), # public
  private = list(),
  active = list(
    )
  )

