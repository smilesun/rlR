# @title  DQN
#
# @format \code{\link{R6Class}} object
# @description Deep Q Network
#
# @section Methods:
# Inherited from \code{AgentArmed}:
# @inheritSection AgentArmed Methods
#
# @return [\code{\link{AgentDQN}}].
AgentDQN = R6::R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    setBrain = function() {
       self$task = "value_fun"
       self$brain = SurroNN$new(self)
       self$model = self$brain
    },

    getXY = function(batchsize) {
        self$list.replay = self$mem$sample.fun(batchsize)
        self$glogger$log.nn$info("replaying %s", self$mem$replayed.idx)
        list.states.old = lapply(self$list.replay, ReplayMem$extractOldState)
        list.states.next = lapply(self$list.replay, ReplayMem$extractNextState)
        self$p.old = self$getYhat(list.states.old)
        self$p.next = self$getYhat(list.states.next)
        list.targets = lapply(1:length(self$list.replay), self$extractTarget)
        self$list.acts = lapply(self$list.replay, ReplayMem$extractAction)
        temp = Reduce(rbind, list.states.old)
        nr = length(list.states.old)
        temp = simplify2array(list.states.old) # R array put elements columnwise
        mdim = dim(temp)
        norder = length(mdim)
        self$replay.x = aperm(temp, c(norder, 1:(norder - 1)))
        self$replay.y = t(simplify2array(list.targets))  # array put elements columnwise
        diff_table = abs(self$replay.y - self$p.old)
        self$replay_delta = apply(diff_table, 1, mean)
    },


    extractTarget = function(i) {
        ins = self$list.replay[[i]]
        act2update =  ReplayMem$extractAction(ins)
        p.old = self$p.old[i, ]
        self$yhat = p.old  # for calculating the  TD error
        r = ReplayMem$extractReward(ins)
        done = ReplayMem$extractDone(ins)
        if (done) {
          target = r
        } else {
          vec.next.Q = self$p.next[i, ]
          a_1 = which.max(vec.next.Q)  # action index start from 1L
          target = r + self$gamma * max(vec.next.Q)
          # equivalent to huber loss
          if (self$clip_td_err) {
            target = max(target, p.old[act2update] - 1L)
            target = min(target, p.old[act2update] + 1L)
          }
        }
        mt = p.old
        mt[act2update] = target  # the not active action arm's Q will not be updated
        #FIXME: shall here be 0?
        # mt[-act2update] = 0.0  # the not active action arm will be set to be zero
        return(mt)
    },

    afterStep = function() {
        if (self$interact$step_in_episode %% self$replay.freq == 0L) {
          self$replay(self$replay.size)
        }
        self$policy$afterStep()
    },

    afterEpisode = function(interact) {
          self$policy$afterEpisode()
          self$mem$afterEpisode()
          self$brain$afterEpisode()
    }
    ) # public
)

AgentDQN$info = function() {
  "Vanilla Deep Q learning"
}

rlR.conf.AgentDQN  =  function() {
  RLConf$new(
          render = FALSE,
          console = TRUE,
          log = FALSE,
          policy.maxEpsilon = 1,
          policy.minEpsilon = 0.01,
          policy.decay.rate = exp(-0.001),
          policy.name = "EpsilonGreedy",
          replay.batchsize = 64L)
}

AgentDQN$test = function() {
  library(rlR)
  env = makeGymEnv("CartPole-v0")
  agent = initAgent("AgentDQN", env)
  agent$learn(200L)
}
