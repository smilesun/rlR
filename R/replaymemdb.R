#' @importFrom magrittr %>% %<>%
ReplayMemDB = R6::R6Class(
  "ReplayMemDB",
  inherit = ReplayMem,
  public = list(
    len = NULL,
    replayed.idx = NULL,
    conf = NULL,
    agent = NULL,
    db.con = NULL,
    table.name = NULL,
    initialize = function(agent, conf) {
      # initialize sqlite connection
      self$db.con = RSQLite::dbConnect(RSQLite::SQLite(), dbname = "replay_memory")    # RSQLite::SQLite() load the driver
      # pick the env name as table name
      self$table.name = agent$env$env %>%
        stringr::str_extract("<([a-z]|[A-Z]|-|[0-9])*>") %>%    # remove the special signs
        stringr::str_remove_all("<|>")   # there's maybe a better solution
      # delete old replay table
      RSQLite::dbExecute( self$db.con, paste0("DROP TABLE IF EXISTS '", self$table.name, "'") )
      # manually create new table to specify primary key - this reduces index search complexity from O(n) to O(log n)
      RSQLite::dbExecute( self$db.con, paste0("
        CREATE TABLE '", self$table.name, "' (
          state_id INTEGER PRIMARY KEY,
          state_old TEXT,
          reward NUMERIC,
          action INTEGER,
          state_new TEXT,
          done INTEGER,
          episode INTEGER,
          stepidx INTEGER,
          info TEXT )
      ") )
      self$len = 0L
      self$conf = conf
      self$agent = agent
    },

    reset = function() {
      RSQLite::dbExecute( self$db.con, paste0("DROP TABLE '", self$table.name, "'") )
      self$len = 0L
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
      self$len = self$len + 1L
      # don't use "." in column names - SQLite will throw up on it
      data.frame(
        state_id   = self$len,
        #state.hash = digest(old_state, algo = "md5"),
        state_old  = state.old,
        reward     = reward,
        action     = action,
        state_new  = state.new,
        done       = done,
        episode    = info$episode,
        stepidx    = info$stepidx,
        info       = if (length(info$info)==0) "NULL" else info$info %>% as.character()
      )
    },

    add = function(ins) {
      # write to sqlite table
      RSQLite::dbWriteTable( self$db.con, self$table.name, ins, append = TRUE)
    },

    afterEpisode = function(interact) {
      # do nothing
    },

    afterStep = function() {
      # do nothing
    },

    getSamples = function(idx) {

      str_to_array = function(string) {

        if (length(self$agent$state_dim) == 1) {   
          # if order of tensor is only 1, which means flat linear state
          strsplit(string, "_")[[1]] %>%     # self defined format of the string, now split it by spliter '_'
            as.numeric() %>%
            array()
        } else if (length(self$agent$state_dim) %in% 2L:3L) {
          change_storage = function(y) {
            storage.mode(y) = "integer"  # change storage type to integer to save space
            y
          }
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
          change_storage %>%    # float storage to int storage
          array(dim = self$agent$state_dim) # this is necessary if state_dim has shape x1 x2 1
          #FIXME: IS THE Orientation of the array right! Critically Important
        }
      }

      replay.samples = paste0("
          SELECT state_old, action, reward, state_new, done, info, episode, stepidx
          FROM '", self$table.name, "'
          WHERE state_id IN (", paste(idx, collapse = ", "), ")
        ") %>%
        RSQLite::dbGetQuery(conn = self$db.con)
      # replay.samples now are the results from the query
      lapply(1:nrow(replay.samples), function(i) list(
        state.old = replay.samples$state_old[i] %>% str_to_array,
        action    = replay.samples$action[i],
        reward    = replay.samples$reward[i],
        state.new = replay.samples$state_new[i] %>% str_to_array,
        done      = replay.samples$done[i],
        info      = list(
          episode = replay.samples$episode[i],
          stepidx = replay.samples$stepidx[i],
          info    = replay.samples$info[i]
        )
      ))
    }
    ),
  private = list(),
  active = list()
)


ReplayMemUniformDB = R6::R6Class("ReplayMemUniformDB",
  inherit = ReplayMemDB,
  public = list(
    sample.fun = function(k) {
      k = min(k, self$len)
      self$replayed.idx = sample(self$len)[1L:k]
      list.res          = self$getSamples(self$replayed.idx)
      return(list.res)
    }
  ),
  private = list(),
  active = list()
  )
