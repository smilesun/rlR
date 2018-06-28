[![Build Status](https://travis-ci.com/smilesun/rlR.svg?branch=master)](https://travis-ci.com/smilesun/rlR)
[![Coverage Status](https://coveralls.io/repos/github/smilesun/rlR/badge.svg?branch=master)](https://coveralls.io/github/smilesun/rlR?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/d0oyb358bh3e8r7r?svg=true)](https://ci.appveyor.com/project/smilesun/rlr)




# rlR: Reinforcement learning in R

## Installation

```r
devtools::install_github("smilesun/rlR", dependencies = TRUE)
```

## Usage

```r
library(rlR)
getDefaultConf()
```

```
##                                         
## render                             FALSE
## log                                FALSE
## console                            FALSE
## agent.gamma                         0.99
## agent.flag.reset.net                TRUE
## agent.lr.decay         0.999000499833375
## agent.lr                           0.001
## agent.store.model                  FALSE
## policy.maxEpsilon                   0.01
## policy.minEpsilon                   0.01
## policy.decay                           1
## policy.softmax.magnify                 1
## replay.memname                   Uniform
## replay.mem.size                    20000
## replay.epochs                          1
## replay.freq                            1
```

```r
env = makeGymEnv("CartPole-v0")
listAvailAgent()
```

```
## [1] "AgentDQN:deep q learning"                      
## [2] "AgentFDQN:frozen target deep q learning"       
## [3] "AgentDDQN: double deep q learning"             
## [4] "AgentPG: policy gradient basic"                
## [5] "AgentPGBaseline: policy gradient with baseline"
## [6] "AgentActorCritic: actor critic method"
```


```r
agent = makeAgent("AgentDQN", env)
```

```
## parameters: 
## -render: - FALSE-
## -log: - FALSE-
## -console: - FALSE-
## -agent.gamma: - 0.99-
## -agent.flag.reset.net: - TRUE-
## -agent.lr.decay: - 0.999000499833375-
## -agent.lr: - 0.001-
## -agent.store.model: - FALSE-
## -policy.maxEpsilon: - 1-
## -policy.minEpsilon: - 0.01-
## -policy.decay: - 0.999000499833375-
## -policy.softmax.magnify: - 1-
## -replay.memname: - Uniform-
## -replay.mem.size: - 20000-
## -replay.epochs: - 1-
## -replay.freq: - 1-
## -policy.name: - ProbEpsilon-
## -replay.batchsize: - 64-
```

```r
system.time({
 perf = agent$learn(150)
})
```

```
##    user  system elapsed 
## 632.296   5.828 631.660
```

```r
 perf$plot()
```

```
## `geom_smooth()` using method = 'loess'
```

![plot of chunk unnamed-chunk-3](inst/figures/dqn.png)
