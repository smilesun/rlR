
AgentFactory = R6Class("AgentFactory",
  public = list(
    )
  )
AgentFactory$genAgent = function(brain, mem, actCnt) {
      f1 = function(...) {
        return(AgentDQN$new(brain = brain, mem = mem, actCnt = actCnt))
      }
      f2 = function(...) {
        return(AgentPG$new(brain = brain, mem = mem, actCnt = actCnt))
      }
      if("SurroPG" %in% class(brain)) {
        AgentPG$new(brain, mem, actCnt)
      }
      else {
        AgentDQN$new(brain, mem, actCnt)
      }
    }


