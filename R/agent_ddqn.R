#' @title 
#' 
#' @description
#' Q_u(S, a; \theta_1) = r + Q_u(S', argmax_a' Q_h(S',a'), \theta_1) + delta
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
AgentDDQN = R6Class("AgentDDQN",
  inherit = AgentDQN,
  public = list(
    brain2 = NULL,
    brain_u = NULL,  # u: to be updated
    brain_h = NULL,  # h: to help
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt, stateCnt, conf)
      self$brain2 = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$dqn, conf$get("agent.nn.arch"))
    },

      toss = function() {
        if (runif(1L) < 0.5) {
          self$brain_u = self$brain
          self$brain_h = self$brain2
        } else {
          self$brain_u = self$brain2
          self$brain_h = self$brain
        }

      },

      replay = function(batchsize) {
          list.x.y = self$getXY(batchsize)
          x = list.x.y$x
          y = list.x.y$y
          self$brain_u$train(x, y)  # update the policy model
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
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain_h$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
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

