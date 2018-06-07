#' @importFrom magrittr %>% %<>%
ReplayMemDB = R6::R6Class(
  "ReplayMemDB",
  inherit = ReplayMem,
  public = list(
    dt = NULL,
    len = NULL,
    replayed.idx = NULL,
    conf = NULL,
    agent = NULL,
    dt.temp = NULL,
    smooth = NULL,
    db.con = NULL,
    table.name = NULL,
    initialize = function(agent, conf) {
      # initialize sqlite connection
      self$db.con = RSQLite::dbConnect(RSQLite::SQLite(), dbname = "replay_memory")
      # pick the env name as table name
      self$table.name = agent$env$env %>%
        stringr::str_extract("<([a-z]|[A-Z]|-|[0-9])*>") %>%
        stringr::str_remove_all("<|>")   # there's maybe a better solution
      self$smooth = rlR.conf4log[["replay.mem.laplace.smoother"]]
      self$dt = data.table()
      self$len = 0L
      self$conf = conf
      self$agent = agent
      # helper constant variable
      self$dt.temp = data.table("delta" = NA, "priorityRank" = NA, "priorityAbs" = NA, "priorityDelta2" = NA, "deltaOfdelta" = NA, "deltaOfdeltaPercentage" = NA)
      self$dt.temp = self$dt.temp[, lapply(.SD, as.numeric)]
    },

    reset = function() {
      RSQLite::dbExecute( self$db.con, paste0("DROP TABLE '", self$table.name, "'") )
      self$dt = data.table()
      self$len = 0L
    },

    mkInst = function(state.old, action, reward, state.new, done, info) {
      # transform/compress states into single string for DB entry
      if (length(self$agent$stateDim) == 1) {
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
        info       = info$episode # TODO: rename "info" to "episode" everywhere
      )
    },

    add = function(ins) {
      # write to sqlite table
      RSQLite::dbWriteTable( self$db.con, self$table.name, ins, append = TRUE )
      mdt = data.table(t(unlist(ins)))
      mdt = cbind(mdt, self$dt.temp)
      self$dt = rbindlist(list(self$dt, mdt), fill = TRUE)
    },

    updateDT = function(idx = NULL) {
      if (is.null(idx)) idx = 1L:self$len
      list.res = self$getSamples(idx)
      td.list = lapply(list.res, self$agent$calculateTDError)
      updatedTDError = unlist(td.list)
      cat(sprintf("mean TD error: %f\n", mean(updatedTDError)))
      old.delta = self$dt[idx, "delta"]
      self$dt[idx, "delta"] = updatedTDError
      self$updatePriority()
    },

    afterEpisode = function(interact) {
      # do nothing
    },

    afterStep = function() {
      # do nothing
    },

    updatePriority = function() {
      self$dt[, "priorityAbs"] =  abs(self$dt[, "delta"]) + self$smooth
      self$dt[, "priorityRank"] = order(self$dt[, "delta"])
    },

    getSamples = function(idx) {

      str_to_array = function(string) {

        if (length(self$agent$stateDim) == 1) {
          strsplit(string, "_")[[1]] %>%
            as.numeric() %>%
            array()
        } else if (length(self$agent$stateDim) %in% 2:4) {
          change_storage = function(y) {storage.mode(y) <- "integer"; y}
          (
            string %>%
              strsplit("") %>%
              .[[1]] %>%
              (function(x) paste0(x[c(TRUE, FALSE)], x[c(FALSE, TRUE)])) %>% #combine to pairs
              as.hexmode %>%   # necessary for correct as.raw
              as.raw %>%       # make it readable as PNG
              (png::readPNG) * 255
          ) %>%
          change_storage
        }
      }

      replay.samples = paste0("
          SELECT state_old, action, reward, state_new, done, info
          FROM '", self$table.name, "'
          WHERE state_id IN (", paste(idx, collapse = ", "), ")
        ") %>%
        RSQLite::dbGetQuery(conn = self$db.con)

      lapply(idx, function(i) list(
        state.old = replay.samples$state_old[i] %>% str_to_array,
        action    = replay.samples$action[i],
        reward    = replay.samples$reward[i],
        state.new = replay.samples$state_new[i] %>% str_to_array,
        done      = replay.samples$done[i],
        info      = replay.samples$info[i]
      ))
    },

    # function taking a list of states (2d/3d/4d arrays) and transforming into video replay_<name>.mp4 in their given order
    # input arrays need at least 2 dimensions
    # mp4 file is compressed -> information loss -> only makes sense for human eyes
    createReplayVideo = function(name, framerate = 25) {
      # check if the mp4 file doesn't exist - otherwise ffmpeg will make issues
      if (length(self$agent$stateDim) == 1) {
        stop("State data format is not suitable for video creation")

      } else if (!file.exists( paste0(getwd(), "/replay_", name, ".mp4")) ) {
        # get all states of the replay memory
        states = self$getSamples(1:self$len) %>%

        # create PNGs in a temporary directory
        tempdir = tempdir()
        for (i in 1:self$len) {
          png::writePNG(
            states[[i]]$state.old / 255,
            target = paste0(tempdir, "/img", stringr::str_pad(i, 7, pad = "0"),".png")
          )
        }
        # use the tool ffmpeg to create a video out of PNGs
        command = paste0(
          "ffmpeg -framerate ", framerate,
          " -i ", tempdir, "'/img%07d.png' -c:v libx264 -pix_fmt yuv420p ",
          getwd(), "/replay_", name, ".mp4"
        )
        system(command)
      } else {
        stop(paste0("The file ", getwd(), "/replay_", name, ".mp4 already exists!"))
      }
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


test.run = function(s) {
  conf = rlR:::RLConf$new(
    render = TRUE,
    console = FALSE,
    log = FALSE,
    policy.maxEpsilon = 1,
    policy.minEpsilon = 0.001,
    policy.decay = exp(-0.001),
    policy.name = "EpsilonGreedy",
    replay.batchsize = 64L,
    replay.memname = "UniformDB",
    agent.nn.arch = list(nhidden = 4, act1 = "relu", act2 = "linear", loss = "mse", lr = 0.00025, kernel_regularizer = "regularizer_l2(l=0.0)", bias_regularizer = "regularizer_l2(l=0.0)"))

  interact = makeGymExperiment(sname = s, aname = "AgentDQN", conf = conf)
  interact$run(1)
}

