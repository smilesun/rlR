context("replay_mem")
test_that("test basic replay_mem works", {
  mem = ReplayMem$new(agent, conf)
  mem$reset()
  ins = mem$mkInst(state.old = array(3), action = 1, reward = 1, state.new = array(4), done = TRUE, info = list())
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
})

context("interact")
test_that("test interact base works", {
  inter = InteractionBase$new()
  expect_error(inter$run())
})
