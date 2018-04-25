#' @title 
#' 
#' @description
#' Q_u(S, a; \theta_1) = r + Q_u(S', argmax_a' Q_h(S',a'), \theta_1) + delta
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
AgentDQL = R6Class("AgentDQL",
  inherit = AgentDQN,
  public = list(
    brain2 = NULL,
    brain_u = NULL,  # u: to be updated
    brain_h = NULL,  # h: to help
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt, stateCnt, conf)
      surro_fun = NNArsenal$makeBrain(self$conf$get("agent.archname"))
      self$brain2 = SurroDQN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = surro_fun)
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
          self$updateDT(x, y)
      },

    #calculateTDError = function(state.old, action, reward, state.new) {
    calculateTDError = function(ins) {
      state.old = ins$state.old
      action = ins$action
      reward = ins$reward
      state.new = ins$state.new
      state.old.tra = array_reshape(state.old, dim = c(1L, dim(state.old)))
      v.old = self$brain_u$pred(state.old.tra)
      ins = ReplayMem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, delta = NULL)
      target = self$extractTarget(ins)
      delta =  target - v.old
      delta = sqrt(mean((delta)^2))
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
          target = r + self$conf$get("agent.gamma") * vec.next.Q.h[a_1]
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

