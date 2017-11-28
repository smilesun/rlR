Instance = R6Class("Instance",
  public = list(
    instance.id = NULL,
    repetition = NULL,
    runtimes = NULL,
    walltime = NULL,
    loss = NULL,
    time.step = NULL,
    features = NULL,
    initialize = function(scenario, instance.id, repetition = 1, loss = identity, time.step = 1) {
      self$instance.id = instance.id
      self$repetition = repetition
      runtimes = data.table(scenario$algo.runs)[repetition == repetition & instance_id == instance.id][,c("algorithm", "runtime", "runstatus")]
      runtimes[, solvetime := runtime]  # the time needed to solve this instance
      runtimes[, runtime := 0]  # new column storing the how much time has been spent
      setkey(runtimes, algorithm)
      self$runtimes = runtimes
      self$walltime = scenario$desc$algorithm_cutoff_time
      self$loss = loss
      self$time.step = time.step
      self$features = data.table(scenario$feature.values)[repetition == repetition &  instance_id == instance.id, !c("instance_id", "repetition")]
    },
    run = function(algo, time = self$time.step) {
      self$runtimes[algo, runtime := min(runtime + time, solvetime)]  # update state: time spent on this instance
      if (self$hit.walltime)
        return(self$loss(self$walltime))
      if (self$is.solved)
        return(sum(unlist(self$runtimes[,runtime])))  # runtime must equal to solve time if the instance is solved
      #print("Instance not solved yet")
      return(NA)  # Instance not solved yet
    }),
  active = list(
    algorithms = function() {
      self$runtimes$algorithm
    },
    is.solved = function() {
      any(self$runtimes[, runtime >= solvetime])  # if any algorithm solve the instance
    },
    hit.walltime = function() {
      any(self$runtimes[, sum(runtime) >= self$walltime])
    },
    has.terminated = function() {
      self$is.solved || self$hit.walltime
    },
    algo.runtimes = function() {
       dcast(self$runtimes[,.(algorithm, runtime)], .~algorithm, value.var = "runtime")[, !"."]
    # long to wide format casting. "." is an alias to list() in data frame
    # value.var names of the column whose values will be filled to be casted
    },
    oracle.step = function() {
      self$runtimes[which.min(solvetime - runtime), .(algorithm)]  # always choose the neareast algorithm which is almost close to be solved
    })
)

