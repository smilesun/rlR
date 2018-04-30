AgentDQN = R6Class("AgentDQN",
  inherit = AgentArmed,
  public = list(
    initialize = function(actCnt, stateCnt, conf) {
       super$initialize(actCnt, stateCnt, conf)
       self$brain = SurroNN$new(actCnt = self$actCnt, stateCnt = self$stateCnt, fun = NNArsenal$dqn, conf$get("agent.nn.arch"))
    },

    extractTarget = function(ins) {
        act2update =  ReplayMem$extractAction(ins)
        old.state = ReplayMem$extractOldState(ins)
        old.state = array_reshape(old.state, dim = c(1L, dim(old.state)))  # array could have scalar dim
        p.old = self$brain$pred(old.state)
        self$yhat = p.old  # for calculating the  TD error
        next.state = ReplayMem$extractNextState(ins)
        next.state = array_reshape(next.state, dim = c(1L, dim(next.state)))
        vec.next.Q = self$brain$pred(next.state)
        a_1 = which.max(vec.next.Q)  # action index start from 1L
        r = ReplayMem$extractReward(ins)
        target = r + self$gamma * max(vec.next.Q)
        mt = p.old
        mt[act2update + 1L] = target  # the not active action arm's Q will not be updated
        return(mt)
    },

    afterStep = function() {
          self$replay(self$replay.size)
    },

    afterEpisode = function(interact) {
        temp = self$epsilon * self$conf$get("policy.decay")
        self$epsilon = max(temp, self$conf$get("policy.minEpsilon"))
        cat(sprintf("Epsilon%f \n", temp))  # same message to console
        self$glogger$log.nn$info("rand steps:%i \n", self$random.cnt)
        cat(sprintf("rand steps:%i \n", self$random.cnt))  # same message to console
        self$random.cnt = 0L
    }
    ), # public
  private = list(),
  active = list(
    )
  )
