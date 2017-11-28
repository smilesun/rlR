##some loss functions ###

par1 = identity
par10 = function(x) x*10

generateTrainingData = function(scenario, time = 20) {

  sc = parseASScenario(scenario)

  instances = unique(sc$algo.runs$instance_id)
  result = list()
  i = 1
  for (instance.id in instances) {
    catf("%i/%i: %s",i, length(instances), instance.id)
    inst = Instance$new(sc, instance.id, loss = par10)
    current.runtimes = inst$algo.runtimes
    data = cbind(inst$oracle.step, current.runtimes)
    #random solver
    while(!inst$has.terminated) {
      a = sample(inst$algorithms, 1)
      res = inst$run(a, time)
      current.runtimes = inst$algo.runtimes
      data = rbind(data, cbind(inst$oracle.step, current.runtimes))
    }
    result[[instance.id]] = cbind(data, inst$features)
    i = i + 1
  }

  return(result)
}

evaluateAgentOnInstance = function(agent, instance, time = 20) {
  while(!instance$has.terminated) {
      a = predict(agent, newdata = cbind(instance$algo.runtimes, inst$features))
      res = instance$run(as.character(a$data[1,1]), time)
    }
  return(res)
}
