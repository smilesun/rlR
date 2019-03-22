AgentPGCompactBL = R6::R6Class("AgentPGCompactBL",
  inherit = AgentPGBaseline,
  public = list(
    p_old_c = NULL,
    p_next_c = NULL,
    delta = NULL,
    list.rewards = NULL,

    setBrain = function() {
      self$task = "policy_fun"
      self$brain_actor = SurroNN$new(self)
    },

     getReplayYhat = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
        self$list.rewards = lapply(self$list.replay, ReplayMem$extractReward)
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        self$model = self$brain_critic
        self$p_old_c = self$getYhat(list.states.old)
        self$p_next_c = self$getYhat(list.states.next)
        temp = simplify2array(list.states.old) # R array put elements columnwise
        mdim = dim(temp)
        norder = length(mdim)
        self$replay.x = aperm(temp, c(norder, 1:(norder - 1)))
    },

     replay = function(batchsize) {
          self$getReplayYhat(batchsize)
          len = length(self$list.replay)   # replay.list might be smaller than batchsize
          self$setAmf(batchsize)
          self$delta = array(self$vec_dis_return, dim = dim(self$p_old_c)) - self$p_old_c
          list.targets.actor = lapply(1:len, function(i) as.vector(self$extractActorTarget(i)))
          list.targets.critic = lapply(1:len, function(i) as.vector(self$extractCriticTarget(i)))
          y_actor = t(simplify2array(list.targets.actor))
          y_actor =  diag(self$amf) %*%  y_actor
          y_actor =  diag(self$delta) %*%  y_actor
          y_critic = array(unlist(list.targets.critic), dim = c(len, 1L))
          self$brain_actor$train(self$replay.x, y_actor)  # update the policy model
          self$brain_critic$train(self$replay.x, y_critic)  # update the policy model
      },

      extractCriticTarget = function(i) {
          y = self$p_old_c[i, ] + self$delta[i]
          return(y)
      },

      extractActorTarget = function(i) {
          act = self$list.acts[[i]]
          delta = (+1.0) * as.vector(self$delta[i])
          #FIXME: interestingly, multiply advantage by -1 also works
          vec.act = rep(0L, self$act_cnt)
          vec.act[act] = 1.0
          target = delta * array(vec.act, dim = c(1L, self$act_cnt))
          return(target)
    },

      adaptLearnRate = function() {
          self$brain_actor$lr =  self$brain_actor$lr * self$lr_decay
          self$brain_critic$lr =  self$brain_critic$lr * self$lr_decay
      },

      afterStep = function() {
          self$policy$afterStep()
      },

      #@override
      evaluateArm = function(state) {
        state = array_reshape(state, c(1L, dim(state)))
        self$vec.arm.q = self$brain_actor$pred(state)
        self$glogger$log.nn$info("state: %s", paste(state, collapse = " "))
        self$glogger$log.nn$info("prediction: %s", paste(self$vec.arm.q, collapse = " "))
      },

    afterEpisode = function(interact) {
      self$replay(self$interact$perf$total_steps)   # key difference here
    }
    ) # public
)
