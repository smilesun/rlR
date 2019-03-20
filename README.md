[![Build Status](https://travis-ci.com/smilesun/rlR.svg?branch=master)](https://travis-ci.com/smilesun/rlR)
[![Coverage Status](https://coveralls.io/repos/github/smilesun/rlR/badge.svg?branch=master)](https://coveralls.io/github/smilesun/rlR?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/d0oyb358bh3e8r7r?svg=true)](https://ci.appveyor.com/project/smilesun/rlr)

[Documentation](https://smilesun.github.io/rlR/)

# rlR: (Deep) Reinforcement learning in R

## Installation

### R package installation

```r
devtools::install_github("smilesun/rlR")
```
or 


```r
devtools::install_github("smilesun/rlR", dependencies = TRUE)
```

rlR use keras with tensorflow as its backend for neural network as functional approximator and OpenAI gym.

[Python Dependencies Installation and Configuration](https://smilesun.github.io/rlR/articles/python_dependencies.html)

## Functional Approximator

### Choose an environment to learn

```r
library(rlR)
env = makeGymEnv("CartPole-v1")
env
```

```
## 
## action cnt: 2 
## state original dim: 4 
## discrete action
```

If you have R package "imager" installed, you could get a snapshot of the environment by

```r
env$snapshot(preprocess = F)
```


### Initialize agent with the environment

```r
agent = initAgent("AgentDQN", env)
agent$learn(200L)  
```

### Look at the performance

```r
agent$plotPerf(F)
```

## More Examples
see [Documentation](https://smilesun.github.io/rlR/)
