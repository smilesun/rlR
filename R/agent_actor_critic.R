#' @title
#'
#' @description
#'
#' @return returndes
#' @export
#' @examples
#' x=c(1,2,3)
AgentActorCritic = R6Class("AgentActorCritic",
  inherit = AgentPG,
  public = list(
    brain_actor = NULL,  # cross entropy loss
    brain_critic = NULL, # mse loss
    initialize = function(actCnt, stateCnt, conf) {
      super$initialize(actCnt, stateCnt, conf = conf)
      self$brain_actor = SurroDQN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$makeNN4PG)
      self$brain_critic = SurroDQN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$makeNN4SV) # single output
      },

     replay = function(batchsize) {
          list.res = self$mem$sample.fun(batchsize)
          list.states = lapply(list.res, ReplayMem$extractOldState)
          list.targets.actor = lapply(list.res, self$extractActorTarget)
          list.targets.critic = lapply(list.res, self$extractCriticTarget)
          x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage
          y_actor = array(unlist(list.targets.actor), dim = c(length(list.targets.actor), self$actCnt))
          y_critic = array(unlist(list.targets.critic), dim = c(length(list.targets.critic), self$actCnt))
          # y = array_reshape(y, dim = c(1L, dim(y)))
          self$brain_actor$train(x, y_actor)  # update the policy model
          self$brain_critic$train(x, y_critic)  # update the policy model
      },

      extractCriticTarget = function(ins) {
          next.state = ReplayMem$extractNextState(ins)
          next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
          next.V = self$brain_critic$pred(next.state)
          r = ReplayMem$extractReward(ins)
          y = r + self$conf$static$agent$GAMMA * next.V
          return(y)
      },

      extractActorTarget = function(ins) {
          act = self$extractAct(ins)
          old.state = ReplayMem$extractOldState(ins)
          old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))
          critic.old.v = self$brain_critic$pred(old.state)
          next.state = ReplayMem$extractNextState(ins)
          next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
          critic.next.v = self$brain_critic$pred(next.state)
          r = ReplayMem$extractReward(ins)
          advantage = r + self$conf$static$agent$GAMMA * critic.next.v - critic.old.v
          vec.act = rep(0L, self$actCnt)
          vec.act[act + 1L] = 1L # the not active action will have exact label
          # target = advantage * array(target, dim = c(1L,self$actCnt)) ? why this ever work with out target defined before?
          target = advantage * array(vec.act, dim = c(1L, self$actCnt))
          return(target)
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
      self$vec.arm.q = self$brain_actor$pred(state)
      self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
    },

    sampleRandomAct = function(state) {
        self$random.action = self$randomAct
    },

    act = function(state) {
      assert(class(state) == "array")
      self$evaluateArm(state)
      action = self$policy(state)
      return(action)
    }
    ), # public
  private = list(),
  active = list(
    )
  )

