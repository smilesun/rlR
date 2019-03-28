# @title Policy Gradient
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
        self$replay.y = t(simplify2array(list_targets))
        # self$replay.y = array(, dim = c(batchsize, self$act_cnt))
    },

    setAmf = function(batchsize) {
        self$setReturn()
        vec_discount = cumprod(rep(self$gamma, batchsize))
        amf = self$vec_dis_return * vec_discount
        amf = self$vec_dis_return
        amf = amf - mean(amf)
        self$amf = amf / sd(amf)
    },

    # replay is executed at the end of episode for each step of the episode, batch size is always set to be the episode length
    replay = function(batchsize) {
        self$setAmf(batchsize)
        self$getXY(batchsize)
        self$replay.y =  diag(self$amf) %*%  self$replay.y
        self$brain$batch_update(self$replay.x, self$replay.y)  # update the policy model
    },

    setReturn = function() {
        episode_idx = self$interact$perf$epi_idx
        self$vec_dis_return = self$interact$perf$list_discount_reward_epi[[episode_idx]]
    },

    #@override
    afterEpisode = function() {
      self$replay(self$interact$perf$total_steps)   # key difference here
      super$afterEpisode()
    }
    ) # public
)

rlR.conf.AgentPG = rlR.conf.AgentPGBaseline = function() {
  RLConf$new(
          agent.lr = 1e-2,
          render = FALSE,
          console = TRUE,
          flag_rescue = FALSE,
          agent.gamma = 0.99,
          policy.maxEpsilon = 0,
          policy.minEpsilon = 0,
          agent.flag.reset.net = FALSE,
          policy.name = "Prob",
          replay.memname = "Latest",
          replay.epochs = 1L)
}

AgentPG$info = function() {
   "Policy Gradient Monte Carlo"
}

AgentPG$test = function() {
  env = makeGymEnv("CartPole-v0")
  conf = getDefaultConf("AgentPG")
  agent = initAgent("AgentPG", env, conf, custom_brain = F)
  agent$learn(200L)
}
