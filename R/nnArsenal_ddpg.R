# normal 1 arm output network with only state as input
createActorNetwork.AgentDDPG.torc = function(state_dim = 3, action_dim = 1L) {
  input_state = keras::layer_input(shape = state_dim)
  states_hidden = input_state %>%
    layer_dense(units = 27, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = 27, activation = "linear") %>%
    layer_dense(units = action_dim, activation = "linear")  # only 1L output!
  model = keras::keras_model(inputs = input_state, outputs = states_hidden2)
  opt = keras::optimizer_adam(lr = 0.0001)
  model %>% compile(
    optimizer = opt,
    loss = "mse",
    metrics = c("accuracy")
    )
  return(list(model = model, input_state = input_state, weights = model$trainable_weights))
}

# both state and action are inputs!
createCriticNetwork.AgentDDPG.torc = function(state_dim, action_dim) {
  input_state = keras::layer_input(shape = state_dim)
  input_action = keras::layer_input(shape = action_dim, name = "input_action")
  action_hidden = input_action %>%
    layer_dense(units = 30, activation = "linear")
  states_hidden = input_state %>%
    layer_dense(units = 30, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = 30, activation = "linear")
  hiddens = keras::layer_add(c(states_hidden2, action_hidden))
  # outputs compose input + dense layers
  predictions = hiddens %>%
    layer_dense(units = 30, activation = "relu") %>%
    layer_dense(units = action_dim, activation = "linear")
  # create and compile model
  model = keras::keras_model(inputs = c(input_action, input_state), outputs = predictions)
  opt = keras::optimizer_adam(lr = 0.0001)
  model %>% compile(
    optimizer = opt,
    loss = "mse",
    metrics = c("accuracy")
    )
  return(list(model = model, input_action = input_action, input_state = input_state))
}


createCriticNetwork.AgentDDPG = function(state_dim, action_dim) {
  input_state = keras::layer_input(shape = state_dim)
  input_action = keras::layer_input(shape = action_dim, name = "input_action")
  action_hidden = input_action %>%
    layer_dense(units = 30, activation = "linear")
  states_hidden = input_state %>% layer_dense(units = 30, activation = "linear")
  hiddens = keras::layer_add(c(states_hidden, action_hidden))
  #concat = keras::layer_concatenate(c(action_hidden, states_hidden))
  hiddens2 = keras::layer_activation_relu(hiddens)

  # outputs compose input + dense layers
  predictions = hiddens2 %>% layer_dense(units = action_dim, activation = "linear")
  # create and compile model
  model = keras::keras_model(inputs = c(input_action, input_state), outputs = predictions)
  opt = keras::optimizer_adam(lr = 0.002)
  model %>% compile(
    optimizer = opt,
    loss = "mse"
    )
  return(list(model = model, input_action = input_action, input_state = input_state))
}



createActorNetwork.AgentDDPG = function(state_dim = 3, action_dim = 1L) {
  input_state = keras::layer_input(shape = state_dim)
  states_hidden = input_state %>%
    layer_dense(units = 30, activation = "relu")
  states_hidden2 = states_hidden %>%
    layer_dense(units = action_dim, activation = "tanh")  # only 1L output!
  model = keras::keras_model(inputs = input_state, outputs = states_hidden2)
  opt = keras::optimizer_adam(0.001)
  model %>% compile(
    optimizer = opt,
    loss = "mse"
    )
  return(list(model = model, input_state = input_state, weights = model$trainable_weights))
}
