ReplayMemPng = R6::R6Class(
  "ReplayMemPng",
  inherit = ReplayMemUniform,
  public = list(
    len = NULL,
    replayed.idx = NULL,
    conf = NULL,
    agent = NULL,
    table.name = NULL,
    initialize = function(agent, conf) {
      super$initialize(agent, conf)
    },

    mkInst = function(state.old, action, reward, state.new, done, info) {
      # transform/compress states into single string for DB entry
      if (length(self$agent$state_dim) == 1) {
        state.old %<>% paste(collapse = "_")
        state.new %<>% paste(collapse = "_")
      } else {
        state.old = (state.old / 255L) %>% (png::writePNG) %>% paste(collapse = "")
        state.new = (state.new / 255L) %>% (png::writePNG) %>% paste(collapse = "")
      }
      super$mkInst(state.old, action, reward, state.new, done, info)
    },

    sample.fun = function(k) {
      k = min(k, self$size)
      #FIXME: the replayed.idx are not natural index, but just the position in the replay memory
      self$replayed.idx = sample(self$len)[5L:k]
      list.res = lapply(self$replayed.idx, function(x) self$samples[[x]])
      replay.samples = list.res
      # replay.samples now are the results from the query

      #FIXME: IS THE Orientation of the array right! Critically Important
      list.replay = lapply(replay.samples, function(x) list(
        state.old = x$state.old %>% str_to_array_h %>% array(dim = self$agent$state_dim),
        action    = x$action,
        reward    = x$reward,
        state.new = x$state.new %>% str_to_array_h %>% array(dim = self$agent$state_dim),
        done      = x$done,
        info      = list(
          episode = x$episode,
          stepidx = x$stepidx,
          info    = x$info
        )
      ))
      list.replay    # DEBUG: self$agent$env$showImage(list.replay[[64]][["state.new"]]) make sense
    }
    ),
  private = list(),
  active = list()
)



change_storage = function(y) {
  storage.mode(y) = "integer"  # change storage type to integer to save space
  y
}

str_to_array_h = function(string) {
  (
    # magittr  require ()
    string %>%
      strsplit("") %>%     # ABEF39 SPLIT into c("A", "B", "E", ...)
      .[[1]] %>%    # return of split is a list
        (function(x) paste0(x[c(TRUE, FALSE)], x[c(FALSE, TRUE)])) %>% #combine to pairs, equivalent to zip:    x[c(TRUE, FALSE)] takes the 1st,3st,5st  and x[c(FALSE, TRUE)] take the 2st, 4st
          as.hexmode %>%   # necessary for correct as.raw. For R to understand this is hexcode other than String.
            as.raw %>%       # make it readable as PNG
              (png::readPNG) * 255   # png package assums image to have range 0-1
      ) %>%
  change_storage    # float storage to int storage
}
