rl_algo = function(job, data, instance, lrn.cl) {
  conf = instance$conf
  interact = makeGymExperiment(conf = conf)
  perf = interact$run()
  return(hash = conf$static$performance$filePrefix, perf = perf)  # key for table join
}
