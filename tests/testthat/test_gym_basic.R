context("gym_basic")
test_that("test Cart-Pole works for each Agent", {
  skip_on_cran()
  agent.names = c("AgentDQN", "AgentFDQN", "AgentDDQN", "AgentPG", "AgentPGBaseline", "AgentActorCritic")
  lapply(agent.names, function(agent.name) {
    tex = paste0(agent.name, sprintf("$test(iter = 3, sname = '%s', render = FALSE)", "CartPole-v0"))
    perf = eval(parse(text = tex))
  })
  expect_true(TRUE)
})
