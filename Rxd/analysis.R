library(BBmisc)
library(data.table)
res = load2("Perf.RData")
length(res$member)
list.res = res$member
list.res[[1]]
col = length(list.res[[1]])

df = as.data.frame(matrix(unlist(list.res), ncol = col, byrow = T))
dim(df)
colnames(df) = c("oracle", "agent", "oraclearm", "agentarm", "instance", "sweep")
head(df)

dt = data.table(df)
