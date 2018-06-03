rl_algo = function(job, data, instance, agent.name) {
  tex = paste0(agent.name, sprintf("$test(iter = 1000, sname = '%s', render = FALSE)", instance))
  perf = eval(parse(text = tex))
  return(perf = perf)  # key for table join
}
