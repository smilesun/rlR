# @title AgentActorCritic
#
# @format \code{\link{R6Class}} object
# @description ActorCritic Agent
#
# @section Methods:
# Inherited from \code{AgentArmed}:
# @inheritSection AgentArmed Methods
#
# @return [\code{\link{AgentActorCritic}}].
AgentActorCritic = R6::R6Class("AgentActorCritic",
  inherit = AgentPGBaseline,
  public = list(
    setBrain = function() {
      self$task = "policy_fun"
      self$brain_actor = SurroNN$new(self)
      self$brain_actor$lr = 0.001
      self$task = "value_fun"
      self$brain_critic = SurroNN$new(self)
      self$brain_critic$lr = 0.01
      self$model = self$brain_critic
    },

    setAmf = function() {
        vec.step = unlist(lapply(self$list.replay, ReplayMem$extractStep))
        vec_discount = sapply(vec.step, function(x) self$gamma^x)
        self$amf = vec_discount
    },

    replay = function(batchsize) {
      self$getReplayYhat(batchsize)  # self$list.rewards are extracted here
      self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
      self$setAmf()
      len = length(self$list.replay)
      list.targets.critic = lapply(1:len, function(i) as.vector(self$extractCriticTarget(i)))
      list.targets.actor = lapply(1:len, function(i) as.vector(self$extractActorTarget(i)))
      y_actor = t(simplify2array(list.targets.actor))
      #y_actor =  diag(self$amf) %*%  y_actor
      y_actor =  self$amf %*%  y_actor
      y_actor =  self$delta %*%  y_actor
      y_critic = array(unlist(list.targets.critic), dim = c(len, 1L))
      self$brain_critic$batch_update(self$replay.x, y_critic)  # first update critic
      self$brain_actor$batch_update(self$replay.x, y_actor)
    },

    extractCriticTarget = function(i) {
      nv = self$gamma * self$p_next_c
      vec.done = unlist(lapply(self$list.replay, ReplayMem$extractDone))
      idx = which(vec.done)
      target = (unlist(self$list.rewards) +  nv)
      if (length(idx) > 0)  target = unlist(self$list.rewards)
      self$delta =  target - self$p_old_c  # Bellman Error as advantage
      return(target)
    },

    afterStep = function() {
      self$policy$afterStep()
      self$replay(1)
    },

    afterEpisode = function(interact) {
      self$policy$afterEpisode()
      self$mem$afterEpisode()
      #if (self$flag_rescue) self$interact$perf$rescue()
      self$brain_actor$afterEpisode()
      self$brain_critic$afterEpisode()
      #self$adaptLearnRate()
    }

    )
  )

AgentActorCritic$test = function() {
  env = makeGymEnv("CartPole-v0")
  agent = initAgent("AgentActorCritic", env)
  agent$learn(2000L)
}
