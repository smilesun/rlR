function() {
  library(profvis)
  profvis(
    {
  agent = initAgent("AgentTable", "CliffWalking-v0")
  agent = initAgent("AgentTable", "FrozenLake-v0")
  agent = initAgent("AgentTable", "Taxi-v2")
  agent$learn(500)
  visualize(agent$q_tab)
  agent$plotPerf(F)
    }
  )
}
