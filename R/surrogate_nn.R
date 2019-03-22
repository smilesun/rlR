SurroNN = R6::R6Class("SurroNN",
  inherit = Surrogate,
  public = list(
    lr = NULL,
    conf = NULL,
    agent = NULL,
    custom_flag = NULL,
    action_input = NULL,
    sess = NULL,
    initialize = function(agent) {
      self$agent = agent
      self$act_cnt = self$agent$act_cnt
      self$custom_flag = FALSE
      self$state_dim = self$agent$state_dim
      self$conf = self$agent$conf
      self$lr = self$conf$get("agent.lr")
      self$sess = agent$sess
      self$model = self$makeModel()
    },

    #@depend self$agent$task
    makeModel = function() {
      do.call(self$agent$network_build_funs[[self$agent$task]], args = list(state_dim = self$state_dim, act_cnt = self$act_cnt))
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

    train_weighted = function(X_train, Y_train, sample_weight) {
      keras::train_on_batch(object = self$model, x = X_train, y = Y_train, sample_weight = sample_weight)
    },

    train = function(X_train, Y_train, batchsize = NULL, epochs = 1L) {
      keras::fit(object = self$model, x = X_train, y = Y_train, batchsize = batchsize, epochs = epochs, verbose = 0)
    },

    batch_update = function(X_train, Y_train) {
      keras::train_on_batch(object = self$model, x = X_train, y = Y_train)
    },

    pred = function(X) {
      res = keras::predict_on_batch(self$model, X)
      res  # FIXME: prediction might be NA from Keras
    },

    afterEpisode = function() {
        lr = keras::k_get_value(self$model$optimizer$lr)
        self$agent$interact$toConsole("learning rate: %s  \n", lr)
        self$lr =  self$lr * self$agent$lr_decay
        keras::k_set_value(self$model$optimizer$lr, self$lr)
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
  )
)


SurroTF = R6::R6Class("SurroTF",
  inherit = SurroNN,
  public = list(
    lr = NULL,
    conf = NULL,
    agent = NULL,
    custom_flag = NULL,
    action_input = NULL,
    sess = NULL,
    tf_states = NULL,
    tf_acts = NULL,
    tf_return = NULL,   # return
    train_op = NULL,
    all_act_prob = NULL,
    makeModel = function() {
        tf = import("tensorflow")
        with(tf$name_scope("inputs"), {
            self$tf_states = tf$placeholder(tf$float32, shape(NULL, self$state_dim), name = "state")
            self$tf_acts = tf$placeholder(tf$int32, shape(NULL), name = "actions_num")
            self$tf_return = tf$placeholder(tf$float32, shape(NULL), name = "actions_value")
        })
        # fc1
        layer = tf$layers$dense(
            inputs = self$tf_states,
            units = 10L,
            activation = tf$nn$tanh,  # tanh activation
            kernel_initializer = tf$random_normal_initializer(mean = 0, stddev = 0.3),
            bias_initializer=tf$constant_initializer(0.1),
            name = 'fc1'
        )
        # fc2
        all_act = tf$layers$dense(
            inputs = layer,
            units = self$act_cnt,
            activation = NULL,
            kernel_initializer = tf$random_normal_initializer(mean = 0, stddev = 0.3),
            bias_initializer = tf$constant_initializer(0.1),
            name = 'fc2'
        )

        self$all_act_prob = tf$nn$softmax(all_act, name = 'act_prob')  # use softmax to convert to probability

        with(tf$name_scope("loss"), {
            neg_log_prob = tf$nn$sparse_softmax_cross_entropy_with_logits(logits = all_act, labels = self$tf_acts)   # this is negative log of chosen action
            #neg_log_prob = tf$reduce_sum(-tf$log(self$all_act_prob) * tf$one_hot(self$tf_acts, self$agent$act_cnt), axis = 1)
            loss = tf$reduce_mean(neg_log_prob * self$tf_return)  # reward guided loss
        })

        with(tf$name_scope("train"), {
            self$train_op = tf$train$AdamOptimizer(self$lr)$minimize(loss)
        })
        self$sess$run(tensorflow::tf$global_variables_initializer())
    },

    batch_update = function(X_train, Y_train) {
        acts = unlist(self$agent$list.acts)
        acts =  acts - 1L
        np = import("numpy")
        acts = np$array(acts)
        #acts = np$reshape(acts, nrow(X_train))
        #np$shape(acts)
        #acts = array(acts, dim = c(nrow(X_train), 1L))
        returns = self$agent$amf
        #returns = array(self$agent$amf, dim = c(nrow(X_train), 1L))
        #returns = np$reshape(returns, nrow(X_train))
        #np$shape(returns)
        returns = np$array(returns)
        #self$sess$run(self$train_op, py_dict(c(self$tf_obs, self$tf_acts, self$tf_vt), c(self$agent$replay.x, acts, returns)))
        self$sess$run(self$train_op, dict("inputs/state:0" = X_train,  "inputs/actions_num:0" = acts, "inputs/actions_value:0" = returns))
    },

    pred = function(X) {
      np = import("numpy")
      obs = np$array(X)
      self$sess$run(self$all_act_prob, dict("inputs/state:0" = obs))
      # py_dict does not work in this case self$sess$run(self$all_act_prob, py_dict(self$tf_obs$name, obs))
    },

    afterEpisode = function() {
    }
  )
)

SurroGrad = R6::R6Class("SurroGrad",
  inherit = SurrogateNN,
  public = list(
   loss2WeightGrad = function(state, action) {
      input = self$model$input
      #input = tf$stop_gradient()
      output = self$model$output
      loss = self$model$total_loss
      #loss$graph$get_collection("variables")
      #loss$graph$get_collection("trainable_variables")
      #loss$graph$get_tensor_by_name(iname)
      #y_ <- tf$placeholder(tf$float32, shape(NULL, self$agent$act_cnt))
      #ph_a = tf$placeholder(tf$float32, c(NULL, 2), name = "action")
      #ph_a = tf$stop_gradient(ph_a)
      targets = self$model$targets
      sample_weights = self$model$sample_weights[[1]]
      #aname = self$model$targets[[1L]]$name
      #cross_entropy = tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(output), reduction_indices=1L))
      weight = self$model$trainable_weights
      tf_grad = keras::k_gradients(loss, weight)
      #tf_grad = tf$gradients(cross_entropy, list(input, targets))
      #tf_grad = tf$gradients(loss, list(input, targets))
      #tf_grad = keras::k_gradients(loss, input)
      #iname = self$model$input$name
      #oname = self$model$output$name
      self$sess$run(tensorflow::tf$global_variables_initializer())
      np = reticulate::import("numpy", convert = FALSE)
      sstate = np$array(state)
      sweights = np$array(rep(1, dim(state)[2L]))
      saction = np$array(action)
      #feed_dict = py_dict(c(iname, aname), c(sstate, saction))
      #feed_dict = py_dict(c(iname, "ph_a"), c(sstate, saction))
      #fun = k_function(list(input, targets), tf_grad)
      #self$model$optimizer$get_gradients(loss, weight)
      feed_dict = py_dict(c(input, targets, sample_weights), c(sstate, saction, sweights))
      self$sess$run(tf_grad, feed_dict)
    },

    getGradients = function(state) {
      yhat = self$pred(state)
      grad = self$loss2WeightGrad(state = state, action = yhat)
    }
    )
)






SurroDDPG = R6::R6Class("SurroDDPG",
  inherit = SurrogateNN,
  public = list(
    makeModel = function() {
        model = self$agent$createBrain()  # the agent itself is responsible for creating the brain
        return(model)
    },

    # calculate gradients with respect to input arm instead of weights
    calGradients2Action = function(state_input, action_input, output = NULL) {
      output = self$model$output
      # FIXME: hard coded here.
      input_action = self$model$input[[1L]]
      input_action_shape = input_action$shape
      tf_grad = keras::k_gradients(output, input_action)  # Returns the gradients of 'variables' w.r.t. 'loss'.
      aname = input_action$name
      sname = self$model$input[[2L]]$name
      oname = self$model$output$name
      #FIXME: do we need initializer here?
      self$sess$run(tensorflow::tf$global_variables_initializer())
      np = reticulate::import("numpy", convert = FALSE)
      sstate = np$array(state_input)
      saction = np$array(action_input)
      feed_dict = py_dict(c(sname, aname), c(sstate, saction))
      #FIXME: do we need to provide the output as well?
      #feed_dict = py_dict(c(sname, aname, oname), c(sstate, saction, output))
      self$sess$run(tf_grad, feed_dict)
    },

    calGradients = function(state, action) {
      output = self$model$output
      input = self$model$trainable_weights
      tf_grad = keras::k_gradients(output, input)
      iname = self$model$input$name
      oname = self$model$output$name
      ph_a = tf$placeholder(tf$float32, c(NULL, 2), name = "action")
      self$sess$run(tensorflow::tf$global_variables_initializer())
      np = reticulate::import("numpy", convert = FALSE)
      sstate = np$array(state)
      saction = np$array(action)
      feed_dict = py_dict(c(iname, "ph_a"), c(sstate, saction))
      self$sess$run(tf_grad, feed_dict)
    },

    loss2WeightGrad= function(state, action) {
      input = self$model$input
      #input = tf$stop_gradient()
      output = self$model$output
      loss = self$model$total_loss
      #loss$graph$get_collection("variables")
      #loss$graph$get_collection("trainable_variables")
      #loss$graph$get_tensor_by_name(iname)
      #y_ <- tf$placeholder(tf$float32, shape(NULL, self$agent$act_cnt))
      #ph_a = tf$placeholder(tf$float32, c(NULL, 2), name = "action")
      #ph_a = tf$stop_gradient(ph_a)
      targets = self$model$targets
      sample_weights = self$model$sample_weights[[1]]
      #aname = self$model$targets[[1L]]$name
      #cross_entropy = tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(output), reduction_indices=1L))
      weight = self$model$trainable_weights
      tf_grad = keras::k_gradients(loss, weight)
      #tf_grad = tf$gradients(cross_entropy, list(input, targets))
      #tf_grad = tf$gradients(loss, list(input, targets))
      #tf_grad = keras::k_gradients(loss, input)
      #iname = self$model$input$name
      #oname = self$model$output$name
      self$sess$run(tensorflow::tf$global_variables_initializer())
      np = reticulate::import("numpy", convert = FALSE)
      sstate = np$array(state)
      sweights = np$array(rep(1, dim(state)[2L]))
      saction = np$array(action)
      #feed_dict = py_dict(c(iname, aname), c(sstate, saction))
      #feed_dict = py_dict(c(iname, "ph_a"), c(sstate, saction))
      #fun = k_function(list(input, targets), tf_grad)
      #self$model$optimizer$get_gradients(loss, weight)
      feed_dict = py_dict(c(input, targets, sample_weights), c(sstate, saction, sweights))
      self$sess$run(tf_grad, feed_dict)
    },

    getGradients = function(state) {
      yhat = self$pred(state)
      grad = self$loss2WeightGrad(state = state, action = yhat)
    }
    )
)



# continous_cross_entropy = function(act_chosen) {
# 
# }
# 
# library(keras)
# quantile <- 0.5
# tilted_loss <- function(q, y, f) {
#   e <- y - f
#   k_mean(k_maximum(q * e, (q - 1) * e), axis = 2)
# }
# 
# sess <- k_get_session()
# ys <- k_constant(c(1,2,3,4), shape = c(2,2))
# yhats <- k_constant(c(1,3,3,4), shape = c(2,2))
# sess$run(tilted_loss(quantile, ys, yhats))

