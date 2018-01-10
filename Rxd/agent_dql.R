#' Q_u(S, a; \theta_1) = r + Q_u(S', argmax_a' Q_h(S',a'), \theta_1) + delta
AgentDQL = R6Class("AgentDQL",
  inherit = AgentDQN,
  public = list(
    brain2 = NULL,
    brain_u = NULL,  # u: to be updated
    brain_h = NULL,  # h: to help
    initialize = function(actionCnt, stateCnt, surro_fun, memname = "latest", policy_fun = "epsilonGreedy") {
      super$initialize(actionCnt, stateCnt, surro_fun, memname = "latest", policy_fun = "epsilonGreedy")
      self$brain2 = SurroDQN$new(actionCnt = actionCnt, stateCnt = stateCnt, fun = surro_fun)
      },

      toss = function() {
        if(runif(1L) < 0.5) {
          self$brain_u = self$brain
          self$brain_h = self$brain2
        } else {
          self$brain_u = self$brain2
          self$brain_h = self$brain
        }

      },

      replay = function(batchsize) {
          list.res = self$mem$sample.fun(batchsize)
          list.states = lapply(list.res, ReplayMem$extractOldState)
          list.targets = lapply(list.res, self$extractTarget)
          x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage
          y = array(unlist(list.targets), dim = c(length(list.targets), self$actCnt))
          self$brain_u$train(x, y)  # update the policy model
      },

    calculateTDError = function(state.old, action, reward, state.new) {
      v.old = self$brain_u$pred(state.old)
      ins = ReplayMem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, delta = NULL)
      target = extractTarget(ins)
      delta =  target - v.old
      return(delta)
    },

    extractTarget = function(ins) {
          old.state = ReplayMem$extractOldState(ins)
          old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))
          yhat = self$brain_u$pred(old.state)
          next.state = ReplayMem$extractNextState(ins)
          next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
          vec.next.Q.u = self$brain_u$pred(next.state)
          vec.next.Q.h = self$brain_h$pred(next.state)
          a_1 = which.max(vec.next.Q.u)  # action index start from 1L
          r = ReplayMem$extractReward(ins)
          target = r + RLConf$static$agent$GAMMA * vec.next.Q.h[a1]
          mt = yhat
          mt[a_1] = target  # the not active action will have exact label
        return(mt)
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = ' '))
      self$vec.arm.q = self$brain_h$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = ' '))
    },

    sampleRandomAct = function(state) {
        self$random.action = self$randomAct
    },

    act = function(state) {
      self$toss()
      assert(class(state) == "array")
      self$evaluateArm(state)
      self$policy(state)
    }
    ), # public
  private = list(),
  active = list(
    )
  )

