#' Base class should only implement the interface
#' Difference between different child class is what is stored in samples(the transition index only or the pair of state)
#' Number of Transitions must be even
ReplayMem = R6::R6Class("ReplayMem",
  public = list(
    samples = NULL,
    capacity = NULL,
    size = NULL,
    len = NULL,
    replayed.idx = NULL,
    conf = NULL,
    agent = NULL,
    observ_stack_len = NULL,
    initialize = function(agent, conf) {
      self$capacity = conf$get("replay.mem.size")
      self$conf = conf
      self$agent = agent
      # observ_stack_len is set via the Environment::setAgent() function
      #' agent = makeAgent("AgentFDQN", env, conf) has incorporated environment into Agent.
      self$reset()
    },

    reset = function() {
      # self$samples = vector(mode = "list", length = self$capacity)  # even without this, the memory won't grow
      self$samples = list()  # can not be fixed size since other replay depends on it.
      self$len = 0L
      self$size = 0L
    },

    #' mkInst can be modified to do preprocessing to state.old
    mkInst = function(state.old, action, reward, state.new, done, info) {
      list(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = info)
    },

    #' usage: AgentBase:Observe()
    #'ins = self$mem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = list(episode = episode, stepidx = stepidx, info = info))
    #'  self$mem$add(ins)
    add = function(ins) {
      pos = (self$len + 1L) %% self$capacity   # self$len can be bigger than capacity
      if (pos == 0) pos = self$capacity  # boundary case if modulo is zero, put new entry at last position
      self$samples[[pos]] = ins  # add samples
      self$len = self$len + 1L  # can be bigger than capacity, len can be converted to float automatically
      self$size = length(self$samples)
    },

    afterEpisode = function(interact) {
      #gc()
      self$agent$interact$toConsole("replaymem size GB:%s \n", as.numeric(object.size(self$samples) / (1024^3)))
      # cat(sprintf("%s\n", pryr::object_size(self$samples)))
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
      # the replayed.idx are not natural index, but just the position in the replay memory, but since we store only transition here, the relative order does not matter.
      self$replayed.idx = sample(self$size)[1L:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      return(list.res)
    }
    )
)

# still store transition, but only the store the index in the state_list
ReplayMemIndex = R6::R6Class("ReplayMemIndex",
  inherit = ReplayMem,
  public = list(
    state_list = NULL,  # only store state
    pos_state_list = NULL,
    state_list_cap = NULL,
    initialize = function(agent, conf) {
      self$pos_state_list = 1L
      super$initialize(agent, conf)
      self$state_list_cap = self$capacity + 1L  # number of states is bigger than number of transitions by 1L
      self$state_list = vector(mode = "list", length = self$state_list_cap)
    },

    # even in case of stacking frame (like pong game), only one new frame is returned by the environment!
    mkInst = function(state.old, action, reward, state.new, done, info) {
      self$state_list[[self$pos_state_list]] = state.old
      self$pos_state_list =  (self$pos_state_list + 1L)  %% self$state_list_cap + 1L
      self$state_list[[(self$pos_state_list + 1L) %% (self$state_list_cap)]] = state.new
      list(state.old = self$pos_state_list, action = action, reward = reward, state.new = (self$pos_state_list + 1L) %% (self$capacity + 1L), done = done, info = info)
    },

    getState = function(x) {
       x$state.old = self$state_list[[x$state.old]]  # x$state.old is now only index!!!
       x$state.new = self$state_list[[x$state.new]]
       return(x)
    },

    sample.fun = function(k) {
      k = min(k, self$size)
      #' no need to remap the replayed.idx since 
      self$replayed.idx = sample(self$size)[1L:k]
      list.res = lapply(self$replayed.idx, function(x) self$getState(self$samples[[x]]))
      return(list.res)
    }
    )
)


#' States are stored sequencially, s_1,s_2,....s_N where N is the capacity The replay memory size is always even number since transitions s_i to s_{i+1} contain 2 states. For Uniform Stack, since s_i,s_i+1 are stacked to form a new state which introduces another level of complexity
#' States should be stored in unit form 0-128 to represent enough information and converted back to float again
#' Note that normalized float can not be converted to int since they reduces either to -1 or +1, which is binarize the image!!!
ReplayMemUniformStack = R6::R6Class("ReplayMemUniformStack",
  inherit = ReplayMemUniform,
  public = list(
    idx_map = NULL,
    # get chronological sample index
    getIdxMap = function(x) {
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

    arr2iarr = function(res) {
      array(as.integer(res), dim = dim(res))  # store integer is less memory hungry
      # storage.mode(res) = "int"
      # res
    },

    #  in agent_base.R
    #' ins = self$mem$mkInst(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = list(episode = episode, stepidx = stepidx, info = info))
    #' self$mem$add(ins)
   mkInst = function(state.old, action, reward, state.new, done, info) {
      list(state.old = self$arr2iarr(state.old), action = action, reward = reward, state.new = self$arr2iarr(state.new), done = done, info = info)
    },

    sample.fun = function(k) {
      k = min(k, self$size)
      sidx = self$observ_stack_len + 1L
      if (length(sidx:self$size) < k) {
        stop("not enough samples in memory")
      }
      #ex: 8-9-1-2(old episode last step)-3(new episode step 1)-4(new episode step 2)-5-6-7 is the replay memory where number represent the chronological order
      self$idx_map = self$getIdxMap()  # chronological index for samples
      self$replayed.idx = sample(sidx:self$size)[1L:k]
      list.res = lapply(self$replayed.idx, function(x) {
        look_back = self$observ_stack_len  # must be able to stack in the same episode 'self$observ_stack_len' number of frames
        res = self$samples[[self$idx_map[x]]]
        step_idx = ReplayMem$extractStep(res)
        ss = step_idx - sidx  # check if step_idx is not smaller than  self$observ_stack_len
        newpos = x  # move the relative sampling position to  new position
        # if at the beginning of an episode, either go forward to later step of the episode or go backward to the last episode ending steps
        if (ss <= 0) {
          newpos = x - ss   # first try to go forward to later steps
          # if at the begin of the episode but at the end of the replay memory
          if (newpos > self$size) {
            newpos = x - step_idx - 1L
          }
          res = self$samples[[self$idx_map[newpos]]]
        }
        vor = (newpos - look_back + 1L)
        adj = self$samples[self$idx_map[vor:newpos]]
        list_state_new = lapply(adj, function(x) {
          x$state.new
        })
        list_state_old = lapply(adj, function(x) {
          x$state.old
        })
        #NOTE: ideally we want to extend the order of the tensor, but keras dense only works with 1d data and conv layer only works with 2d, so an alternative is to stack the array in the 3rd dimension
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
