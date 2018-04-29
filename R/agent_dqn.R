AgentDQN = R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    list.acts = NULL,
    initialize = function(actCnt, stateCnt, conf) {
       super$initialize(actCnt, stateCnt, conf)
       self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$dqn, conf$get("agent.nn.arch"))
    },

    replay = function(batchsize) {
        list.x.y = self$getXY(batchsize)
        x = list.x.y$x
        y = list.x.y$y
        self$brain$train(x, y)  # update the policy model
        # self$updateDT(x, y)
    },

    extractTarget = function(ins) {
        act2update =  ReplayMem$extractAction(ins)
        old.state = ReplayMem$extractOldState(ins)
        old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))  # array could have scalar dim
        p.old = self$brain$pred(old.state)
        # self$glogger$log.nn$info("extract target:old state: %s", old.state)
        # self$glogger$log.nn$info("extract target:old state Q: %s", p.old)
        self$yhat = p.old  # for calculating the  TD error
        next.state = ReplayMem$extractNextState(ins)
        next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
        vec.next.Q = self$brain$pred(next.state)
        a_1 = which.max(vec.next.Q)  # action index start from 1L
        r = ReplayMem$extractReward(ins)
        target = r + self$gamma * max(vec.next.Q)
        mt = p.old
        mt[act2update + 1L] = target  # the not active action arm's Q will not be updated
        # self$glogger$log.nn$info("extract target:target: %s", mt)
        return(mt)
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    },

    sampleRandomAct = function(state) {
        self$random.action = self$randomAct
    }
    ), # public
  private = list(),
  active = list(
    )
  )

