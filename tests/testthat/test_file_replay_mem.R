context("replay_mem")
test_that("test basic replay_mem works", {
  conf = rlR.conf.DQN()
  env = rlR::Environment$new()
  env$overview()
  agent = initAgent("AgentFDQN", env, conf)
  mem = ReplayMem$new(agent, conf)
  mem$reset()
  ins = mem$mkInst(state.old = array(rep(1, 4)), action = c(1, 2), reward = 1, state.new = array(rep(2, 4)), done = TRUE, info = list())
  mem$add(ins)
  expect_class(mem, "ReplayMem")
})

test_that("test stack replay_mem works", {
  # conf = rlR.conf.DQN()
  # conf$set(replay.memname = "UniformStack", replay.mem.size = 70L)  # bigger than batchsize
   env = rlR::Environment$new()
  env$overview()
  env = makeGymEnv("Pong-v0", observ_stack_len = 4L, state_preprocess = list(fun = subsample))
  agent = initAgent("AgentFDQN", env, conf)
  makeArray = function(i) array(rep(i, 61*80*4), dim = c(61,80,4))
  mem = agent$mem
  mem$reset()
  for (i in 1:70) {
    ins = mem$mkInst(state.old = makeArray(i-1), action = 1, reward = i, state.new = makeArray(i), done = TRUE, info = list(episode = 1, stepidx = i))
    mem$add(ins)
  }
  for (i in 71:140) {
    ins = mem$mkInst(state.old = makeArray(i-1), action = 1, reward = i, state.new = makeArray(i), done = TRUE, info = list(episode = 2, stepidx = i))
    mem$add(ins)
    res = mem$sample.fun(64)
    a = sapply(res, function(x) x$info$stepidx)
    e = sapply(res, function(x) x$info$episode)
    b = sapply(res, function(x) x$state.new[1])
    expect_true(all(a - b == 3L))
  }
  expect_class(mem, "ReplayMem")
})


# test_that("test uniformStack_mem works", {
#   skip_on_cran()
#   conf = rlR.conf.DQN()
#   conf$set(replay.memname = "UniformStack", replay.mem.size = 70L)  # bigger than batchsize
#   env = makeGymEnv("Pong-v0", repeat_n_act = 400, observ_stack_len = 2, state_preprocess = list(fun = subsample))
#   env$overview()
#   agent = initAgent("AgentFDQN", env, conf)
#   agent$learn(3)
#   expect_class(agent, "AgentFDQN")
# })
# 
context("interact")
test_that("test interact base works", {
  inter = InteractionBase$new()
  expect_error(inter$run())
})
