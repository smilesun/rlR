rl_algo = function(job, data, instance, agent.name) {
  conf = rlR::RLConf$new(
           render = FALSE,
           policy.epsilon = 1,
           policy.minEpsilon = 0,
           policy.decay = exp(-0.05),
           policy.name = "EpsilonGreedy",
           replay.batchsize = 5L,
           replay.memname = "PrioritizedAbs",
           agent.nn.arch = list(nhidden = 64, act1 = "sigmoid", act2 = "linear", loss = "mse", lr = 0.00005, kernel_regularizer = "regularizer_l2(l=0.000001)", bias_regularizer = "regularizer_l2(l=0.000011)"))

  interact = rlR::makeGymExperiment(sname = instance, aname = agent.name, conf = conf)
  perf = interact$run(data$iteration)
  return(perf = perf)  # key for table join
}
