SurroNN = R6Class("SurroNN",
  inherit = Surrogate,
  public = list(
    initialize = function(actCnt, stateDim, arch.list) {
      self$actCnt = actCnt
      self$stateDim = stateDim
      if (length(self$stateDim) > 1L) {
        self$model = makeCnn(input_shape = self$stateDim, act_cnt = self$actCnt)
      } else {
        self$model = makeKerasModel(input_shape = self$stateDim, output_shape = self$actCnt, arch.list = arch.list)
      }
    },

    getWeights = function() {
      keras::get_weights(self$model)
    },

    train = function(X_train, Y_train, epochs = 1L) {
      nr = nrow(X_train)
      lr = keras::k_get_value(self$model$optimizer$lr)
      keras::fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0)
    },

    pred = function(X) {
      res = self$model %>% predict(X)
      res  # prediction might be NA from Keras
    }
    ),
  private = list(),
  active = list()
  )

SurroNN4PG = R6Class("SurroNN4PG",
  inherit = SurroNN,
  public = list(
    lr = NULL,
    initialize = function(actCnt, stateDim, arch.list) {
      super$initialize(actCnt, stateDim, arch.list)
      self$lr = arch.list[["lr"]]
    },

    train = function(X_train, Y_train, epochs = 1L) {
          nr = nrow(X_train)
          keras::k_set_value(self$model$optimizer$lr, self$lr / nr)
          keras::fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0)
        }
    )
  )
