# several base class that do not have too many lines of code
Surrogate= R6Class("Surrogate",
  public = list(
      actionCnt = NULL,
      stateCnt = NULL,
      model = NULL,
    initialize = function(actionCnt, stateCnt) {
      self$actionCnt = actionCnt
      self$stateCnt = stateCnt
      self$model = self$createModel(input_shape = stateCnt, output_shape = actionCnt)  # proxy method
    },

    train = function(X_train, Y_train, epochs = EPOCH){
      stop("not implmented!")
},

    persist = function(path) {
      temp = self$clone()
      save(temp, file = path)
    },

    pred = function(X) {
      stop("not implemented")
    }
    ),
  private = list(),
  active = list()
  )


Interaction = R6Class("Interaction",
  public = list(
    rl.agent = NULL,
    rl.env = NULL,
    perf = NULL,
    maxiter = NULL,
    run = function() {
      stop("not implemented")
    }
    ), # public
  private = list(),
  active = list()
  )

Performance = R6Class("Performance",
  public = list(
    list.reward.epi = NULL,
    list.rewardPerEpisode = NULL,
    list.discountedRPerEpisode = NULL,
    list.stepsPerEpisode = NULL,
    epi.idx = NULL,

    initialize = function() {
      self$list.reward.epi = list()
      self$epi.idx = 0L
      self$list.rewardPerEpisode = list()
      self$list.discountedRPerEpisode = list()
      self$list.stepsPerEpisode = list()
    },

    toString = function() {
      print(self$list.reward.epi)
      #print(self$epi.idx)
      #print(self$list.rewardPerEpisode)
      #print(self$list.discountedRPerEpisode)
      print("steps per episode")
      print(self$list.stepsPerEpisode)
    },

    observe = function() {

    }
    ),
  private = list(),
  active = list())

Environment = R6Class("Environment",
  public = list(
    env = NULL,
    initialize = function(env) {
    },
    step = function(action) {
    }
    ),
  private = list(),
  active = list()
  )

