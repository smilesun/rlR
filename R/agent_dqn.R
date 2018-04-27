AgentDQN = R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    list.acts = NULL,
    initialize = function(actCnt, stateCnt, conf) {
       super$initialize(actCnt, stateCnt, conf)
       surro_fun = NNArsenal$makeBrain(self$conf$get("agent.archname"))
       self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = surro_fun, conf$get("agent.nn.arch"))
    },
 
    getXY = function(batchsize) {
        list.res = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        for (i in self$mem$replayed.idx) {
          self$glogger$log.nn$info("%s", self$mem$samples[[i]])
        }
        list.states = lapply(list.res, ReplayMem$extractOldState)
        list.targets = lapply(list.res, self$extractTarget)  # target will be different at each iteration for the same experience
        self$list.acts = lapply(list.res, ReplayMem$extractAction)
        # x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage, bug point is it changes the orientation of the replay memory, but works for mountain car, with batchsize 5
        # y = array(unlist(list.targets), dim = c(length(list.targets), self$actCnt))
        x = as.array(t(as.data.table(list.states)))  # array put elements columnwise
        y = rbindlist(lapply(list.targets, as.data.table))
        y = as.data.frame(y)
        y = as.matrix(y)
        return(list(x = x, y = y))
    },

    replay = function(batchsize) {
        list.x.y = self$getXY(batchsize)
        # debug: self$brain$pred(x)
        x = list.x.y$x
        y = list.x.y$y
        self$brain$train(x, y)  # update the policy model
        self$updateDT(x, y)
    },

    extractTarget = function(ins) {
        act2update =  ReplayMem$extractAction(ins)
        old.state = ReplayMem$extractOldState(ins)
        old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))
        p.old = self$brain$pred(old.state)
        self$glogger$log.nn$info("old state q: %s", p.old)
        self$yhat = p.old  # for calculating the  TD error
        next.state = ReplayMem$extractNextState(ins)
        next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
        vec.next.Q = self$brain$pred(next.state)
        a_1 = which.max(vec.next.Q)  # action index start from 1L
        r = ReplayMem$extractReward(ins)
        target = r + self$gamma * max(vec.next.Q)
        mt = p.old
        #wrong:mt[a_1] = target  # the not active action will have exact label
        mt[act2update +1L] = target  # the not active action will have exact label
        self$glogger$log.nn$info("target: %s", mt)
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
    },

    act = function(state) {
      assert(class(state) == "array")
      self$evaluateArm(state)  # calculation will be used for the policy to decide which arm to use
      act = self$policy(state)  # returning the chosen action
      self$glogger$log.nn$info("action: %d", act)
      return(act)
    }
    ), # public
  private = list(),
  active = list(
    )
  )

