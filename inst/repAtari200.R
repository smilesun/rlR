library(rlR)
conf = getDefaultConf("AgentFDQN")
conf$set(replay.batchsize = 32,
  replay.freq = 1L,
  console = TRUE,
  agent.lr.decay = 1,
  agent.lr = 0.00025,
  agent.update.target.freq = 1e4,
  replay.memname = "Png",
  render = F,
  policy.minEpsilon = 0.1,
  agent.start.learn = 5e4L,
  policy.aneal.steps = 1e6,
  replay.mem.size = 1e6,
  log = FALSE,
  agent.clip.td = TRUE,
  policy.decay.type = "decay_linear")

makeCnnCritic = function(state_dim, act_cnt) {
  require("keras")
  text = paste("model <- keras_model_sequential();",
  'model %>%',
  ' layer_conv_2d(filter = 16, kernel_size = c(8,8), strides = c(4, 4), 
  padding = "same", input_shape = state_dim) %>%',
    'layer_activation("relu") %>%',
    'layer_conv_2d(filter = 32, kernel_size = c(4,4), strides = c(2, 2)) %>%',
    'layer_activation("relu") %>%',
    'layer_flatten() %>%',
    'layer_dense(256) %>%',
    'layer_activation("relu") %>%',
    'layer_dense(act_cnt) %>%',
    'layer_activation("linear");',
    'opt <- optimizer_rmsprop(lr = 0.00025);',
    'model %>% compile(loss = "mse", optimizer = opt, metrics = "accuracy")')
  model = eval(parse(text = text))
  return(model)
}

library(doParallel)
cl = makeCluster(5)
registerDoParallel(cl)
res = repExperiment(sname = "Seaquest-v0", aname = "AgentFDQN", conf = conf, nrep = 5, nepi = 200, value_fun = makeCnnCritic, observ_stack_len = 2L, state_preprocess = list(fun = rlR:::subsample))
