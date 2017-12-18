AgentActorCritic = R6Class("AgentActorCritic",
  inherit = AgentPG,
  public = list(
    brain_actor = NULL,  # cross entropy loss
    brain_critic = NULL, # mse loss
    initialize = function(actionCnt, stateCnt, surro_fun, memname = "latest", policy_fun = "epsilonGreedy") {
      super$initialize(actionCnt, stateCnt, surro_fun, memname = "latest", policy_fun = "epsilonGreedy")
      self$brain_actor = SurroDQN$new(actionCnt = actionCnt, stateCnt = stateCnt, fun = NNArsenal$makeNN4PG)
      self$brain_critic = SurroDQN$new(actionCnt = actionCnt, stateCnt = stateCnt, fun = NNArsenal$makeNN4SV) # single output
      },

     replay = function(batchsize) {
          list.res = self$mem$sample.fun(batchsize)
          list.states = lapply(list.res, self$extractOldState)
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
          next.state = self$extractNextState(ins)
          next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
          next.V = self$brain_critic$pred(next.state)
          r = self$extractReward(ins)
          y = r + RLConf$static$agent$GAMMA * next.V
          return(y)
      },

      extractActorTarget = function(ins) {
          act = self$extractAct(ins)
          old.state = self$extractOldState(ins)
          old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))
          old.v = self$brain_critic$pred(old.state)
          next.state = self$extractNextState(ins)
          next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
          next.V = self$brain_critic$pred(next.state)
          r = self$extractReward(ins)
          advantage = r + RLConf$static$agent$GAMMA * next.V - old.V
          temp = rep(0L,self$actCnt)
          temp[act + 1L] = 1L # the not active action will have exact label
          target = advantage * array(target, dim = c(1L,self$actCnt))
          return(target)
    },

    evaluateArm = function(state) {
      state = array_reshape(state, c(1L, dim(state)))
      log.nn$info("state: %s", paste(state, collapse = ' '))
      self$vec.arm.q = self$brain_actor$pred(state)
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

