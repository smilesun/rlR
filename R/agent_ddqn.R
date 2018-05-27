#' @title Double Q learning
#'
#' @format \code{\link{R6Class}} object
#' @description
#' A \code{\link{R6Class}} to represent Double Deep Q learning Armed Agent
#' %$Q_u(S, a; \theta_1) = r + Q_u(S', argmax_a' Q_h(S',a'), \theta_1) + delta$
#' @section Methods:
#' Inherited from \code{AgentArmed}:
#' @inheritSection AgentArmed Methods
#'
#' @return [\code{\link{AgentDDQN}}].
#' @export
AgentDDQN = R6::R6Class("AgentDDQN",
  inherit = AgentDQN,
  public = list(
    brain2 = NULL,
    brain_u = NULL,  # u: to be updated
    brain_h = NULL,  # h: to help
    p.next.h = NULL,
    initialize = function(env, conf) {
      super$initialize(env, conf)
      self$setBrain()
    },

    setBrain = function() {
      super$setBrain()
      self$brain2 = SurroNN$new(actCnt = self$actCnt, stateDim = self$stateDim, arch.list = self$conf$get("agent.nn.arch"))
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

      getXY = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
        self$model = self$brain_u
        self$p.old = self$getYhat(list.states.old)
        self$p.next = self$getYhat(list.states.next)
        self$model = self$brain_h
        self$p.next.h = self$getYhat(list.states.next)
        list.targets = lapply(1:length(self$list.replay), self$extractTarget)
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        self$replay.x = as.array(t(as.data.table(list.states.old)))  # array put elements columnwise
        self$replay.y = as.array(t(as.data.table(list.targets)))  # array put elements columnwise
    },


      replay = function(batchsize) {
          self$getXY(batchsize)
          self$brain_u$train(self$replay.x, self$replay.y)
      },

    extractTarget = function(i) {
          ins = self$list.replay[[i]]
          act2update =  ReplayMem$extractAction(ins)
          yhat = self$p.old[i, ]
          vec.next.Q.u = self$p.next[i, ]
          vec.next.Q.h = self$p.next.h[i, ]
          a_1 = which.max(vec.next.Q.h)
          r = ReplayMem$extractReward(ins)
          done = ReplayMem$extractDone(ins)
          if (done) {
            target = r
          } else {
            target = r + self$gamma * vec.next.Q.u[a_1]
          }
          mt = yhat
          mt[act2update] = target
        return(mt)
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain_h$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    },

    act = function(state) {
      self$toss()
      assert(class(state) == "array")
      self$evaluateArm(state)
      self$policy$act(state)
    }
    ), # public
  private = list(),
  active = list(
    )
  )

AgentDDQN$test = function(iter = 500L, sname = "CartPole-v0", render = TRUE) {
  conf = RLConf$new(
           render = render,
           policy.maxEpsilon = 1,
           policy.decay = exp(-0.001),
           policy.name = "EpsilonGreedy",
           replay.batchsize = 64L,
           agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))
  interact = makeGymExperiment(sname = sname, aname = "AgentDDQN", conf = conf)
  perf = interact$run(iter)
  return(perf)
}
