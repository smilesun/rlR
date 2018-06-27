#' @title  DQN
#'
#' @format \code{\link{R6Class}} object
#' @description Deep Q Network
#'
#' @section Methods:
#' Inherited from \code{AgentArmed}:
#' @inheritSection AgentArmed Methods
#'
#' @return [\code{\link{AgentDQN}}].
#' @export
AgentDQN = R6::R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    initialize = function(env, conf = NULL) {
       if (is.null(conf)) conf = rlR.conf.DQN()
       super$initialize(env, conf)
       self$setBrain()
    },

    setBrain = function() {
       self$brain = SurroNN$new(self)
       self$model = self$brain
    },

    # user creation of brain from outside
    customizeBrain = function(obj) {
       self$brain$setModel(obj)
       self$model = self$brain
    },

    makeCnn = function()  {
      return(makeCnnCritic(input_shape = self$stateDim, act_cnt = self$act_cnt))
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
        }
        mt = p.old
        mt[act2update] = target  # the not active action arm's Q will not be updated
        return(mt)
    },

    afterStep = function() {
        if (self$interact$idx.step %% self$replay.freq == 0) {
          self$replay(self$replay.size)
          self$policy$afterStep()
        }
    },

    afterEpisode = function(interact) {
          self$policy$afterEpisode()
          self$mem$afterEpisode()
          self$brain$lr =  self$brain$lr * self$lr_decay
          self$brain$afterEpisode()
    }
    ), # public
  private = list(),
  active = list(
    )
  )

rlR.conf.DQN = function() {
  RLConf$new(
          render = FALSE,
          console = FALSE,
          log = FALSE,
          policy.maxEpsilon = 1,
          policy.minEpsilon = 0.01,
          policy.decay = exp(-0.001),
          policy.name = "ProbEpsilon",
          replay.batchsize = 64L,
          agent.nn.arch = list(nhidden = 64, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))
}

AgentDQN$test = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  env = makeGymEnv(sname)
  agent = makeAgent("AgentDQN", env)
  agent$updatePara(console = console, render = render)
  agent$learn(iter)
}

AgentDQN$test2 = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  #env = makeGymEnv("MountainCar-v0", act.cheat = function(a) { if(a ==2) return(3); return(a)}, actcnt = 2)
  env = makeGymEnv("MountainCar-v0")
  agent = makeAgent("AgentDQN", env)
  agent$updatePara(console = TRUE, render = TRUE,  log = TRUE, policy.maxEpsilon = 1, policy.minEpsilon = 0.1, policy.decay = exp(-0.01), replay.batchsize = 8, agent.nn.arch = list(nhidden = 8, act1 = "relu", act2 = "linear", loss = "mse", lr = 1e-3, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))
  agent$learn(1000)
}

AgentDQN$test3 = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  env = makeGymEnv("MountainCar-v0")
  agent = makeAgent("AgentDQN", env)
  model = keras_model_sequential()
  model %>% layer_dense(units = 10, activation = 'relu', input_shape = c(2)) %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 3, activation = 'linear');model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 9e-4))
  model
  agent$updatePara(console = TRUE, render = TRUE,  log = TRUE, policy.maxEpsilon = 0.15, policy.minEpsilon = 0.05, policy.decay = exp(-0.001), replay.batchsize = 10, replay.epochs = 4, agent.lr.decay = exp(-0.001), agent.gamma = 0.95)
  agent$customizeBrain(model)
  agent$learn(1000)
}

AgentDQN$test4 = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  env = makeGymEnv("MountainCar-v0", act.cheat = function(a) { if(a ==2) return(3); return(a)}, actcnt = 2)
  agent = makeAgent("AgentDQN", env)
  model = keras_model_sequential()
  model %>% layer_dense(units = 10, activation = 'relu', input_shape = c(2)) %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 2, activation = 'linear');model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 9e-4))
  model
  agent$updatePara(console = TRUE, render = TRUE,  log = TRUE, policy.maxEpsilon = 0.15, policy.minEpsilon = 0.05, policy.decay = exp(-0.001), replay.batchsize = 10, replay.epochs = 4, agent.lr.decay = exp(-0.001), agent.gamma = 0.95)
  agent$customizeBrain(model)
  agent$learn(1000)
}

AgentDQN$testcnn = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  env = makeGymEnv("Pong-v0", act_cheat = c(3, 4))
  agent = makeAgent("AgentDQN", env)
  agent$updatePara(replay.batchsize = 32, render = TRUE, replay.freq = 4L)
  agent$learn(1)
}

AgentDQN$testpongram = function(iter = 1000L, sname = "CartPole-v0", render = FALSE, console = FALSE) {
  env = makeGymEnv("Pong-ram-v0", act_cheat = c(2, 3))
  agent = makeAgent("AgentDQN", env)
  agent$learn(1)
}
