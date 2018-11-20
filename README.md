[![Build Status](https://travis-ci.com/smilesun/rlR.svg?branch=master)](https://travis-ci.com/smilesun/rlR)
[![Coverage Status](https://coveralls.io/repos/github/smilesun/rlR/badge.svg?branch=master)](https://coveralls.io/github/smilesun/rlR?branch=master)
[![Build status](https://ci.appveyor.com/api/projects/status/d0oyb358bh3e8r7r?svg=true)](https://ci.appveyor.com/project/smilesun/rlr)

# rlR: Deep Reinforcement learning in R

## Installation

### R package installation

```r
devtools::install_github("smilesun/rlR")
```
or 


```r
devtools::install_github("smilesun/rlR", dependencies = TRUE)
```

rlR itself use tensorflow as its backend for neural network as functional approximator, so python dependency is needed. 

### Configure to connect to python
To run the examples,  you need to have the python packages `numpy-1.14.5`, `tensorflow-1.8.0`, `keras-2.1.6`, `gym-0.10.5` installed in the **same** python path. 

This python path can be your system default python path or a virtual environment(either system python virtual environment or anaconda virtual environment).

Other package versions might also work but not tested.

To look at all python paths you have, in a R session, run

```r
reticulate::py_discover_config()
```

Check which is your system default python:

```r
Sys.which("python")
```

If you want to use a python path other than this system default, run the following(replace the '/usr/bin/python' with the python path you want) before doing anything else with reticulate.

```r
reticulate::use_python("/usr/bin/python", required=TRUE)
```
**"Note that you can only load one Python interpreter per R session so the use_python call only applies before you actually initialize the interpreter."** Which means if you changed your mind, you have to close the current R session and open a new R session.

Confirm from the following if the first path is the one you wanted

```r
reticulate::py_config()
```

### Python dependencies installation by rlR function
It is not recommended to mix things up with the system python, so by default, the rlR facility will install the dependencies to virtual environment named 'r-tensorflow' either to your system virtualenv or Anaconda virtualenv.

For Unix user
- Ensure that you have **either** of the following available
  - Python Virtual Environment: 
    
    ```bash
    pip install virtualenv
    ```
  - Anaconda
- Install dependencies through 
  - if you have python virtualenv available:
    
    ```r
    rlR::installDep2SysVirtualEnv(gpu = FALSE)
    ```
  - if you have anaconda available:
    
    ```r
    rlR::installDepConda(conda_path = "auto", gpu = FALSE)
    ```

For Windows user
- Ensure that you have Anaconda available.
- Install dependencies through `{r eval=FALSE} rlR::installDepConda(gpu = FALSE)` 

If you want to have gpu support, simply set the gpu argument to be true in the function call.

### Mannual python dependency installation
You can also install python dependencies without using rlR facility function, for example, you can open an anaconda virtual environment  'r-tensorflow' by

```bash
source activate r-tensorflow`
pip install gym
pip install cmake
pip install gym[atari]
```


## Usage

```r
library(rlR)
listGymEnvs()[1L:10L]
```

```
##  [1] "DoubleDunk-ramDeterministic-v4" "DoubleDunk-ramDeterministic-v0"
##  [3] "Robotank-ram-v0"                "CartPole-v0"                   
##  [5] "CartPole-v1"                    "Asteroids-ramDeterministic-v4" 
##  [7] "Pooyan-ram-v4"                  "Gopher-ram-v0"                 
##  [9] "HandManipulateBlock-v0"         "Pooyan-ram-v0"
```

```r
env = makeGymEnv("CartPole-v1")
env$overview()
```

```
## 
## action cnt: 2 
## state original dim: 4 
## discrete action
```

```r
listAvailAgent(env)
```

```
## $AgentDQN
## [1] "Deep Q learning"
## 
## $AgentFDQN
## [1] "Frozen Target Deep Q Learning"
## 
## $AgentDDQN
## [1] "Double Deep QLearning"
## 
## $AgentPG
## [1] "Policy Gradient Monte Carlo"
## 
## $AgentPGBaseline
## [1] "Policy Gradient with Baseline"
## 
## $AgentActorCritic
## [1] "Actor Critic Method"
```


```r
options(width=1000)
listAvailConf()[, .(name, note, name)]
```

```
##                         name                                                                                                                      note                     name
##  1:                   render                                                                                    Whether to show rendering video or not                   render
##  2:                      log                                                                             Whether to log important information on drive                      log
##  3:                  console                                                                            Whether to enable debug info output to console                  console
##  4:              agent.gamma                                                                             The discount factor in reinforcement learning              agent.gamma
##  5:     agent.flag.reset.net                                                                                      Whether to reset the neural network      agent.flag.reset.net
##  6:           agent.lr.decay                                                                        The decay factor of the learning rate at each step           agent.lr.decay
##  7:                 agent.lr                                                                                               learning rate for the agent                 agent.lr
##  8:        agent.store.model                                                                            whether to store the model of the agent or not        agent.store.model
##  9: agent.update.target.freq                                                                                How often should the target network be set agent.update.target.freq
## 10:        agent.start.learn                                                                            after how many transitions should replay begin        agent.start.learn
## 11:            agent.clip.td                                                                                                  whether to clip TD error            agent.clip.td
## 12:        policy.maxEpsilon                                                                                      The maximum epsilon exploration rate        policy.maxEpsilon
## 13:        policy.minEpsilon                                                                                      The minimum epsilon exploration rate        policy.minEpsilon
## 14:        policy.decay.rate                                                                                                            the decay rate        policy.decay.rate
## 15:        policy.decay.type                                                        the way to decay epsion, can be decay_geo, decay_exp, decay_linear        policy.decay.type
## 16:       policy.aneal.steps how many steps needed to decay from maximum epsilon to minmum epsilon, only valid when policy.decay.type = 'decay_linear'       policy.aneal.steps
## 17:   policy.softmax.magnify                                                                                                                      <NA>   policy.softmax.magnify
## 18:         replay.batchsize                                                                     how many samples to take from replay memory each time         replay.batchsize
## 19:           replay.memname                                                                                                 The type of replay memory           replay.memname
## 20:          replay.mem.size                                                                                             The size of the replay memory          replay.mem.size
## 21:            replay.epochs                                                               How many gradient decent epochs to carry out for one replay            replay.epochs
## 22:              replay.freq                                                                                   how many steps to wait until one replay              replay.freq
##                         name                                                                                                                      note                     name
```

```r
conf = getDefaultConf("AgentDQN")
conf$show()
```

```
##                                      value
## render                               FALSE
## log                                  FALSE
## console                              FALSE
## agent.gamma                           0.99
## agent.flag.reset.net                  TRUE
## agent.lr.decay           0.999000499833375
## agent.lr                             0.001
## agent.store.model                    FALSE
## agent.update.target.freq               200
## agent.start.learn                       64
## agent.clip.td                        FALSE
## policy.maxEpsilon                        1
## policy.minEpsilon                     0.01
## policy.decay.rate        0.999000499833375
## policy.decay.type                decay_geo
## policy.aneal.steps                   1e+06
## policy.softmax.magnify                   1
## replay.batchsize                        64
## replay.memname                     Uniform
## replay.mem.size                      20000
## replay.epochs                            1
## replay.freq                              1
## policy.name                    ProbEpsilon
```

```r
conf$set(render = FALSE, console = FALSE)   # Since this file is generated by Rmarkdown, we do not want other output message to blur the markdown file.
```


```r
agent = makeAgent("AgentDQN", env, conf)
ptmi = proc.time()
perf = agent$learn(200L)  
proc.time() - ptmi
```


```r
agent$plotPerf()
```
![plot of chunk mplot](inst/figures/mplot-1.png)
