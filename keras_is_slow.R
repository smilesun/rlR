# # keras is too slow...
# library(reinforcelearn)
# library(keras)
#
# # build very simple model (basically just a table)
# model = keras_model_sequential()
# model %>% layer_dense(units = 4, activation = "linear", input_shape = c(16),
#   use_bias = FALSE, kernel_initializer = "zeros")
# keras::compile(model, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.1))
# model %>% get_weights()
#
# # gridworld with 16 states
# envir = gridworld(shape = c(4, 4), goal.states = c(0), initial.state = 15)
#
# n.steps = 1000
# envir$reset()
#
# # simple qlearning
# for (i in seq_len(n.steps)) {
#   s = reinforcelearn::nHot(envir$state + 1, len = 16)
#   action.val.state = predict(model, s)
#   policy = reinforcelearn:::getPolicy(action.val.state, epsilon = 0.1)
#   a = reinforcelearn:::sampleActionFromPolicy(policy)
#   envir$step(a)
#
#   s.n = reinforcelearn::nHot(envir$state + 1, len = 16)
#   action.val.next.state = predict(model, s.n)
#   target = envir$reward + max(action.val.next.state)
#   y = action.val.state
#   y[a + 1] = target
#   # gradient descent step
#   keras::fit(model, s, y, verbose = 0, epochs = 4L)#, steps_per_epoch = 1L)
#   # model %>% get_weights()
#   if (envir$done) {
#     print(paste("Episode finished after", envir$n.steps, "steps."))
#     envir$reset()
#   }
# }
# # learns the optimal solution
# matrix(round(apply(Q[[1]], 1, max), 1), nrow = 4)


## some benchmarking ----
f = function() {
  s = reinforcelearn::nHot(envir$state + 1, len = 16)
  Q1 = predict(model, s)
  policy = reinforcelearn:::getPolicy(Q1, epsilon = 0.1)
  a = reinforcelearn:::sampleActionFromPolicy(policy)
  envir$step(a)
  Q.n = predict(model, reinforcelearn::nHot(envir$state + 1, len = 16))
  target = envir$reward + max(Q.n)
  y = Q1
  y[a + 1] = target
  # keras::fit(model, s, y, verbose = 0, epochs = 4L)
}

# microbenchmark::microbenchmark(f())
# one training step takes around 22 milliseconds
# 100 steps in this small example take 2 seconds!!
# when commenting out keras::fit it needs 6 milliseconds
