SurroNN = R6::R6Class("SurroNN",
  inherit = Surrogate,
  public = list(
    lr = NULL,
    arch.list = NULL,
    conf = NULL,
    agent = NULL,
    custom_flag = NULL,
    action_input = NULL,
    sess = NULL,
    initialize = function(agent, arch_list_name = "agent.nn.arch", ...) {
      par_list = list(...)
      self$agent = agent
      self$actCnt = self$agent$actCnt
      self$custom_flag = FALSE
      if ("act_cnt" %in% names(par_list)) self$actCnt = par_list[["act_cnt"]]
      self$stateDim = self$agent$stateDim
      self$conf = self$agent$conf
      if (!is.null(self$conf)) {
        self$arch.list = self$conf$get(arch_list_name)
        self$arch.list$lr = self$conf$get("agent.lr")
        self$lr = self$arch.list$lr
      }
      self$model = self$makeModel()
      self$sess = tensorflow::tf$Session()
    },

    makeModel = function() {
      if (self$agent$env$flag_continous) {
        model = self$agent$createBrain()  # the agent itself is responsible for creating the brain
        return(model)
      }
      if (length(self$stateDim) > 1L) {
        model = makeCnn(input_shape = self$stateDim, act_cnt = self$actCnt)
      } else {
        model = makeKerasModel(input_shape = self$stateDim, output_shape = self$actCnt, arch.list = self$arch.list)
      }
      return(model)
    },

    # calculate gradients with respect to input arm instead of weights
    calGradients2Action = function(state_input, action_input) {
      output = self$model$output
      input = self$action_input
      tf_grad = keras::k_gradients(output, input)
      iname = self$action$name
      oname = self$model$output$name
      self$sess$run(tensorflow::tf$global_variables_initializer())
      np = reticulate::import("numpy", convert = FALSE)
      sstate = np$array(state_input)
      saction = np$array(action_input)
      feed_dict = py_dict(c(iname, oname), c(sstate, saction))
      self$sess$run(tf_grad, feed_dict)
    },

    calGradients = function(state, action) {
      output = self$model$output
      input = self$model$trainable_weights
      tf_grad = keras::k_gradients(output, input)
      iname = self$model$input$name
      oname = self$model$output$name
      self$sess$run(tensorflow::tf$global_variables_initializer())
      np = reticulate::import("numpy", convert = FALSE)
      sstate = np$array(state)
      saction = np$array(action)
      feed_dict = py_dict(c(iname, oname), c(sstate, saction))
      self$sess$run(tf_grad, feed_dict)
    },

    getGradients = function(state) {
      res = self$pred(state)
      grad = self$calGradients(state = state, action = res)
    },

    setModel = function(obj) {
      self$model = obj
      self$custom_flag = TRUE
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
      keras::fit(object = self$model, x = X_train, y = Y_train, epochs = epochs, verbose = 0)
    },

    pred = function(X) {
      res = keras::predict_on_batch(self$model, X)
      res  # FIXME: prediction might be NA from Keras
    },

    afterEpisode = function() {
        #FIXME: adjust learning rate with dataframe nrow?
        keras::k_set_value(self$model$optimizer$lr, self$lr)
        lr = keras::k_get_value(self$model$optimizer$lr)
        self$agent$interact$toConsole("learning rate: %s  \n", lr)
    }
    ),
  private = list(
    deep_clone = function(name, value) {
      # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for each field, with the name and value.
      if (name == "model") {
        weights = self$getWeights()
        if (self$custom_flag) {
          model = keras::clone_model(self$model)
        } else {
          model = self$makeModel()
        }
        keras::set_weights(model, weights)
        return(model)
      } else {
        # For all other fields, just return the value
        value
      }
    }
  ),
  active = list()
)
