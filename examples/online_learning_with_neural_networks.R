# Online reinforcement learning with neural networks
# Comparison of keras and mxnet

library(reinforcelearn)
library(keras)
library(mxnet)

source("R/environment.R")
source("R/gridworld.R")

# gridworld with 16 states
envir = gridworld(shape = c(4, 4), goal.states = c(0), initial.state = 15)

n.steps = 2000

## keras --------------

# build very simple model (basically just a table)
model.keras = keras_model_sequential()
model.keras %>% layer_dense(units = 4, activation = "linear", input_shape = c(16),
  use_bias = FALSE, kernel_initializer = "zeros")
keras::compile(model.keras, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.4))
model.keras %>% get_weights()

# simple qlearning
envir$reset()
for (i in seq_len(n.steps)) {
  s = reinforcelearn::nHot(envir$state + 1, len = 16)
  action.val.state = predict(model.keras, s)
  policy = reinforcelearn:::getPolicy(action.val.state, epsilon = 0.1)
  a = reinforcelearn:::sampleActionFromPolicy(policy)
  envir$step(a)
  s.n = reinforcelearn::nHot(envir$state + 1, len = 16)
  action.val.next.state = predict(model.keras, s.n)
  target = envir$reward + max(action.val.next.state)
  y = action.val.state
  y[a + 1] = target
  keras::fit(model.keras, s, y, verbose = 0, epochs = 1L)#, steps_per_epoch = 1L)
  if (envir$done) {
    print(paste("Episode finished after", envir$n.steps, "steps."))
    envir$reset()
  }
}

# learns very slowly, but converges to optimal solution
model.keras %>% get_weights()

#----------------------
## mxnet --------------

data = mx.symbol.Variable("data")
fc1 = mx.symbol.FullyConnected(data, num_hidden = 4, no.bias = TRUE)
lro = mx.symbol.LinearRegressionOutput(fc1)

# first (fake) data observation
train.x = matrix(0, ncol = 16)
train.y = matrix(0, nrow = 4)

# first training step
model.mxnet = mx.model.FeedForward.create(lro,
  X = train.x, y = train.y, initializer = mx.init.uniform(0.001),
  num.round = 1, array.batch.size = 1, array.layout = "rowmajor",
  learning.rate = 0.4, eval.metric = mx.metric.mae)

print(model.mxnet$arg.params)

# open question: how do I initialize weights exactly to 0?

# simple qlearning
envir$reset()
for (i in seq_len(n.steps)) {
  s = reinforcelearn::nHot(envir$state + 1, len = 16)
  action.val.state = predict(model.mxnet, s, array.layout = "rowmajor")
  policy = reinforcelearn:::getPolicy(action.val.state, epsilon = 0.1)
  a = reinforcelearn:::sampleActionFromPolicy(policy)
  envir$step(a)
  s.n = reinforcelearn::nHot(envir$state + 1, len = 16)
  action.val.next.state = predict(model.mxnet, s.n, array.layout = "rowmajor")
  target = envir$reward + max(action.val.next.state)
  y = action.val.state
  y[a + 1] = target
  model.mxnet = mx.model.FeedForward.create(symbol = model.mxnet$symbol,
    arg.params = model.mxnet$arg.params, aux.params = model.mxnet$aux.params,
    X = s, y = y, begin.round = i + 1, num.round = i + 2,
    array.batch.size = 1, array.layout = "rowmajor",
    learning.rate = 0.4, eval.metric = mx.metric.mae, verbose = FALSE)
  if (envir$done) {
    print(paste("Episode finished after", envir$n.steps, "steps."))
    envir$reset()
  }
}

# learns also a bit slowly, but converges to optimal solution
print(model.mxnet$arg.params)


#----------------------
## some benchmarking ----
keras = function() {
  envir$reset()
  s = reinforcelearn::nHot(envir$state + 1, len = 16)
  action.val.state = predict(model.keras, s)
  policy = reinforcelearn:::getPolicy(action.val.state, epsilon = 0.1)
  a = reinforcelearn:::sampleActionFromPolicy(policy)
  envir$step(a)
  s.n = reinforcelearn::nHot(envir$state + 1, len = 16)
  action.val.next.state = predict(model.keras, s.n)
  target = envir$reward + max(action.val.next.state)
  y = action.val.state
  y[a + 1] = target
  keras::fit(model.keras, s, y, verbose = 0, epochs = 1L)
}

mxnet = function() {
  envir$reset()
  s = reinforcelearn::nHot(envir$state + 1, len = 16)
  action.val.state = predict(model.mxnet, s, array.layout = "rowmajor")
  policy = reinforcelearn:::getPolicy(action.val.state, epsilon = 0.1)
  a = reinforcelearn:::sampleActionFromPolicy(policy)
  envir$step(a)
  s.n = reinforcelearn::nHot(envir$state + 1, len = 16)
  action.val.next.state = predict(model.mxnet, s.n, array.layout = "rowmajor")
  target = envir$reward + max(action.val.next.state)
  y = action.val.state
  y[a + 1] = target
  model.mxnet = mx.model.FeedForward.create(symbol = model.mxnet$symbol,
    arg.params = model.mxnet$arg.params, aux.params = model.mxnet$aux.params,
    X = s, y = y, begin.round = 2, num.round = 3,
    array.batch.size = 1, array.layout = "rowmajor",
    learning.rate = 0.4, eval.metric = mx.metric.mae, verbose = FALSE)
}

microbenchmark::microbenchmark(keras(), mxnet(), times = 100)
# keras: one training step takes around 24 milliseconds
#   100 steps in this small example take more than 2 seconds!!
#   when commenting out keras::fit it needs 9 milliseconds
# ---
# mxnet: one training step takes around 13 milliseconds
#   when commenting out mx.model.FeedForward.create it needs 7 milliseconds
