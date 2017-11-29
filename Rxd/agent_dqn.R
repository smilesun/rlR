# @depend rf.R
AgentDQN = R6Class("AgentDQN",
  inherit = Agent,
  public = list(
    initialize = function(actionCnt, stateCnt, fun, memname = "latest") {
       self$epsilon = RLConf$static$EPSILON
       self$brain = SurroDQN$new(actionCnt = actionCnt, stateCnt = stateCnt, fun = fun)
       self$mem = ReplayMem$factory(memname)()
       self$actCnt = actionCnt
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
        target = r + RLConf$static$GAMMA * max(vec.next.Q)
        mt = p.old
        mt[a_1] = target  # the not active action will have exact label
        return(mt)
    },

    act = function(state) {
      assert(class(state) == "array")
      state = array_reshape(state, c(1L, dim(state)))
      log.nn$info("state: %s", paste(state, collapse = ' '))
      vec.q = self$brain$pred(state)
      log.nn$info("prediction: %s", paste(vec.q, collapse = ' '))
      action = which.max(vec.q) - 1L  # always use OpenAI gym convention
      log.nn$info("chosen %d", action)
      if(runif(1L) < self$epsilon) {
        a2 = self$randomAct
        log.nn$info("random chosen %d", a2)
        return(a2)
      }
      return(action)
    }
    ), # public
  private = list(),
  active = list(
    )
  )

