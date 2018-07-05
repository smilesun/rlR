context("replay_mem")
test_that("test basic replay_mem works", {
  conf = rlR.conf.DQN()
  mem = ReplayMem$new(agent, conf)
  mem$reset()
  env = rlR::Environment$new()
  env$overview()
  agent = makeAgent("AgentFDQN", env, conf)
  ins = mem$mkInst(state.old = array(rep(1, 4)), action = c(1, 2), reward = 1, state.new = array(rep(2, 4)), done = TRUE, info = list())
  mem$add(ins)
  expect_class(mem, "ReplayMem")
})

test_that("test replay_mem works", {
  skip_on_cran()
  conf = rlR.conf.DQN()
  conf$set(replay.memname = "UniformStack", replay.mem.size = 70L)  # bigger than batchsize
  env = makeGymEnv("Pong-v0", repeat_n_act = 400, observ_stack_len = 2)
  env$overview()
  agent = makeAgent("AgentFDQN", env, conf)
  expect_class(agent, "AgentFDQN")
})

context("interact")
test_that("test interact base works", {
  inter = InteractionBase$new()
  expect_error(inter$run())
})
