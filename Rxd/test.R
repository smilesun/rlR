library(aslib)
library(R6)
library(data.table)
library(mlr)

source("helpers.R")
source("Instance.R")

path = "/home/janek/projects/aslib_data/BNSL-2016/" # Adapt

scenario = parseASScenario(normalizePath(path))
inst = Instance$new(scenario, "abscisic_bdeu-0.1-2", loss = par10)
instances = unique(sc$algo.runs$instance_id)

if (!file.exists("../data/training_data_BNSL-2016.rds") {
  data = generateTrainingData(path)
  saveRDS(data, file = "../data/training_data_BNSL-2016.rds")
} else {
  data = readRDS("../data/training_data_BNSL-2016.rds")
}


train = makeClassifTask(data = do.call(rbind, data[1:200]), target = "algorithm")
test = do.call(rbind, data[201:1179])



## evaluation to predict optimal step ###
lrn = makeLearner("classif.ranger")
mod = train(lrn, train)

preds = predict(mod, newdata = test)

performance(preds, acc)


##evaluate instance
sc = parseASScenario(scenario)

agent.oracle.perf = lapply(201:250, function(i) {
  inst = Instance$new(sc, names(data)[i], loss = par10)
  agent = evaluateAgentOnInstance(mod, inst)
  oracle = min(inst$runtimes$solvetime)
  c(agent = agent, oracle = oracle)
})


agent.oracle.perf = do.call(rbind, agent.oracle.perf)
