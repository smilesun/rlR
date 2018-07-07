ReplayMem = R6::R6Class("ReplayMem",
  public = list(
    samples = NULL,
    capacity = NULL,
    size = NULL,
    len = NULL,
    start_idx = NULL,
    end_idx = NULL,
    replayed.idx = NULL,
    conf = NULL,
    agent = NULL,
    observ_stack_len = NULL,
    initialize = function(agent, conf) {
      self$capacity = conf$get("replay.mem.size")
      self$conf = conf
      self$agent = agent
      # observ_stack_len is set via the Environment::setAgent() function
      self$reset()
    },

    reset = function() {
      self$start_idx = 1L
      self$end_idx = 1L
      self$samples = list()
      self$len = 0L
      self$size = 0L
    },

    mkInst = function(state.old, action, reward, state.new, done, info) {
      #FIXME: benchmark if it saves time to seperately store sars
      list(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = info)
    },

    add = function(ins) {
      pos = (self$len + 1L) %% self$capacity   # self$len can be bigger than capacity
      if (pos == 0) pos = self$capacity  # boundary case if modulo is zero, put new entry at last position
      self$samples[[pos]] = ins  # add samples
      self$len = self$len + 1L  # can be bigger than capacity
      self$size = length(self$samples)
    },

    afterEpisode = function(interact) {
      cat(sprintf("replaymem size GB:%s \n", as.numeric(object.size(self$samples)) / (1024^3)))
    },

    afterStep = function() {
      # do nothing
    }
    )
)

ReplayMemUniform = R6::R6Class("ReplayMemUniform",
  inherit = ReplayMem,
  public = list(
    sample.fun = function(k) {
      k = min(k, self$size)
      #FIXME: the replayed.idx are not natural index, but just the position in the replay memory
      self$replayed.idx = sample(self$size)[1L:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    }
    )
)

ReplayMemUniformStack = R6::R6Class("ReplayMemUniformStack",
  inherit = ReplayMemUniform,
  public = list(
    getIdxMap = function (x) {
      if (self$len <= self$capacity) {
        return(1L:self$len)
      }
      pos = self$len %% self$capacity
      if (pos == 0L) {
        return(1L:self$len)
      }
      istart = pos + 1L
      iend = pos
      return(c(istart:self$capacity, 1L:iend))
    },

    add = function(ins) {
      mdim = self$agent$env$state_dim[1L:2L]
      ins$state.old = array_reshape(ins$state.old[, , 1L], c(mdim, 1L))
      ins$state.new = array_reshape(ins$state.new[, , 1L], c(mdim, 1L))
      super$add(ins)
    },

    sample.fun = function(k) {
      k = min(k, self$size)
      #FIXME: the replayed.idx are not natural index, but just the position in the replay memory
      sidx = self$observ_stack_len + 1L
      if (length(sidx:self$size) < k) {
        stop("not enough samples in memory")
      }
      idx_map = self$getIdxMap()
      self$replayed.idx = sample(sidx:self$size)[1L:k]
      list.res = lapply(self$replayed.idx, function(x) {
        look_back = self$observ_stack_len
        res = self$samples[[idx_map[x]]]
        step_idx = ReplayMem$extractStep(res)
        ss = step_idx - sidx
        newpos = x
        # if at the beginning of an episode, either go forward or go backward to the last episode
        if (ss <= 0) {
          newpos = x - ss   # first try to go forward to later steps
          # if at the begin of the episode but at the end of the replay memory
          if (newpos > self$size) {
            newpos = x - step_idx - 1L
          }
          res = self$samples[[idx_map[newpos]]]
        }
        vor = (newpos - look_back + 1L)
        adj = self$samples[idx_map[vor:newpos]]
        list_state_new = lapply(adj, function(x) {
          x$state.new
        })
        list_state_old = lapply(adj, function(x) {
          x$state.old
        })
        #NOTE: ideally we want to extend the order of the tensor, but keras dense only works with 1d data and conv layer only works with 2d, so an alternative is to stack the array
        res$state.new = abind::abind(list_state_new)
        res$state.old = abind::abind(list_state_old)
        res
      })
      return(list.res)
    }
    )
)


ReplayMemLatest = R6::R6Class("ReplayMemLatest",
  inherit = ReplayMem,
  public = list(
   sample.fun = function(k) {
      # k is always set to the episode length currently
      k = min(k, self$size)  # when k is too small, the learning stops at particular step
      self$replayed.idx = (self$size - k + 1L): self$size
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    },

    afterStep = function() {
      # do nothing
    },

    afterEpisode = function() {
      self$reset()
    })
)

ReplayMemOnline = R6::R6Class("ReplayMemOnline",
  inherit = ReplayMemLatest,
  public = list(
  sample.fun = function(k) {
      # k is always set to the episode length currently
      k = min(k, self$size)  # when k is too small, the learning stops at particular step
      self$replayed.idx = (self$size - k + 1L): self$size
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      self$reset()
      return(list.res)
    }
  )
)

makeReplayMem = function(name, agent, conf) {
  all = getNamespaceExports("rlR")
  mem.idx = which(sapply(all, function(x) grepl("ReplayMem", x)))
  # assert(paste0("ReplayMem", name) %in% all[mem.idx])
  tex = sprintf("ReplayMem%s$new(agent = agent, conf = conf)", name)
  mem = eval(parse(text = tex))
  return(mem)
}
