SurroNN = R6::R6Class("SurroNN",
  inherit = Surrogate,
  public = list(
    lr = NULL,
    initialize = function(actCnt, stateDim, arch.list) {
      self$actCnt = actCnt
      self$stateDim = stateDim
      self$lr = arch.list$lr
      if (length(self$stateDim) > 1L) {
        self$model = makeCnn(input_shape = self$stateDim, act_cnt = self$actCnt)
      } else {
        self$model = makeKerasModel(input_shape = self$stateDim, output_shape = self$actCnt, arch.list = arch.list)
      }
    },

    getWeights = function() {
      keras::get_weights(self$model)
    },

    setWeights = function(weights) {
      keras::set_weights(self$model, weights)
    },

    persist = function(file_path) {
      keras::save_model_hdf5(object = self$model, file_path = file_path)
    },

    train = function(X_train, Y_train, epochs = 1L) {
      #nr = nrow(X_train)
      #keras::k_set_value(self$model$optimizer$lr, self$lr / 3)
      #lr = keras::k_get_value(self$model$optimizer$lr)
      #cat(sprintf("learning rate: %s", lr))
      keras::fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0)
    },

    pred = function(X) {
      res = keras::predict_on_batch(self$model, X)
      res  # FIXME: prediction might be NA from Keras
    },

    afterEpisode = function() {
        #nr = nrow(X_train)
        # keras::k_set_value(self$model$optimizer$lr, self$lr / nr)
        keras::k_set_value(self$model$optimizer$lr, self$lr)
        lr = keras::k_get_value(self$model$optimizer$lr)
        cat(sprintf("learning rate: %s  \n", lr))
    }

    ),
  private = list(
    deep_clone = function(name, value) {
      # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
      # each field, with the name and value.
      if (name == "model") {
        # `a` is an environment, so use this quick way of copying
        #list2env(as.list.environment(value, all.names = TRUE),
                 #parent = emptyenv())
        keras::clone_model(self$model)
      } else {
        # For all other fields, just return the value
        value
      }
    }
  ),
  active = list()
)

SurroNN4PG = R6::R6Class("SurroNN4PG",
  inherit = SurroNN,
  public = list(
    lr = NULL,
    initialize = function(actCnt, stateDim, arch.list) {
      super$initialize(actCnt, stateDim, arch.list)  # FIXME: arch.list could be None when PG surrogate is called as super prior to PGBaseline is called.
      self$lr = arch.list[["lr"]]
    },

    train = function(X_train, Y_train, epochs = 1L) {
          keras::fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0)
    },

    afterEpisode = function() {
        #nr = nrow(X_train)
        # keras::k_set_value(self$model$optimizer$lr, self$lr / nr)
        keras::k_set_value(self$model$optimizer$lr, self$lr)
        lr = keras::k_get_value(self$model$optimizer$lr)
        cat(sprintf("learning rate: %s  \n", lr))
    }
    )
  )
