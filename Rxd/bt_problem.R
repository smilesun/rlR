# create configuration object
rl_prob = function(job, data) {
  conf = RLConf$new()
  dnames = names(data)
  # lapply(dnames, function(x) conf$updatePara(x, names(data[[x]]))
  # conf$updatePara("gym", "agentname", "A3C")
  return(list(conf = conf))
}

