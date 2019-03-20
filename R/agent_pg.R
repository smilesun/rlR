# @title Policy Gradient
#
# @format \code{\link{R6Class}} object
# @description Policy Gradient
#
# @section Methods:
# Inherited from \code{AgentArmed}:
# @inheritSection AgentArmed Methods
#
# @return [\code{\link{AgentPG}}].
AgentPG = R6::R6Class("AgentPG",
  inherit = AgentArmed,
  public = list(
    flag_rescue = NULL,
    amf = NULL,
    initialize = function(env, conf) {
      self$flag_rescue = conf$get("agent.flag.reset.net")
      super$initialize(env, conf = conf)
    },

    setBrain = function() {
      self$task = "policy_fun"
      self$brain = SurroNN$new(self)
      self$model = self$brain
    },

    extractTarget = function(ins) {
        act =  ReplayMem$extractAction(ins)
        vec_act = rep(0.0, self$act_cnt)
        vec_act[act] =  +1.0
        return(vec_act)
    },

    # loss = -\sum_k{(y_k\log(yhat_k)}, \frac{\partial loss}{\partial \yhat} = -\sum_k{y_k\frac{yhat_k}{yhat_k}} = -policy gradient
    #@override
    getXY = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list_states_old = lapply(self$list.replay, ReplayMem$extractOldState)
        list_targets = lapply(self$list.replay, self$extractTarget)
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        arr_states_old = simplify2array(list_states_old)
        norder = length(dim(arr_states_old))
        self$replay.x = aperm(arr_states_old, c(norder, 1:(norder - 1)))
        self$replay.y = array(simplify2array(list_targets), dim = c(batchsize, self$act_cnt))
    },

    setAmf = function(batchsize) {
        self$setReturn()
        vec_discount = cumprod(rep(self$gamma, batchsize))
        amf = self$vec_dis_return * vec_discount
        amf = amf - mean(amf)
        self$amf = amf / sqrt(sum(amf ^ 2))
    },

    # replay is executed at the end of episode for each step of the episode, batch size is always set to be the episode length
    replay = function(batchsize) {
        self$setAmf(batchsize)
        self$getXY(batchsize)
        self$brain$train(self$replay.x, self$replay.y * self$amf)  # update the policy model
    },

    setReturn = function() {
        episode_idx = self$interact$perf$epi_idx
        self$vec_dis_return = self$interact$perf$list_discount_reward_epi[[episode_idx]]
    },

    #@override
    afterEpisode = function(interact) {
      self$replay(self$interact$perf$total_steps)   # key difference here
      super$afterEpisode()
    }
    ) # public
)

AgentPG$test = function() {
  set.seed(1)
  library(rlR)
  library(magrittr)
  library(keras)
  env = makeGymEnv("CartPole-v0")
  conf = getDefaultConf("AgentPG")
  conf$set(console = T, policy.name = "EpsilonGreedy", policy.maxEpsilon = 0.001)
  agent = initAgent("AgentPG", env, conf)
  agent$customizeBrain(list(policy_fun = rlR:::makePolicyNet))
  agent$learn(200L)
}

AgentPGTF = R6::R6Class("AgentPGTF",
  inherit = AgentPG,
  public = list(
    flag_rescue = NULL,
    amf = NULL,
    gradients = function(interact) {
        self$setReturn(interact)  # total.step is calculated here
        self$getXY(self$interact$perf$total.step)
        vec_discount = cumprod(rep(self$gamma, interact$perf$total.step))
        amf = self$vec_dis_return * vec_discount
        amf = amf - mean(amf)
        amf = amf / sqrt(sum(amf ^ 2))
        agg = self$brain$getGradients(array(self$replay.x[1, ], dim = c(1, self$state_dim)))
        for (i in 2: dim(self$replay.x)[1L]) {
           grads = self$brain$getGradients(array(self$replay.x[i, ], dim = c(1, self$state_dim)))
           grads = lapply(grads, function(x) x * amf[i])
           agg = lapply(length(agg), function(i) agg[[i]] + grads[[i]])
        }
        agg
        self$model$model$optimizer$apply_gradients
        self$model$model$optimizer$get_updates(self$replay.x[1, ])
        names(self$model$model$optimizer)
        self$model$model$optimizer$from_config
    }
  )
)
