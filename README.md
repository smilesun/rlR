# rlR: Reinforcement learning in R


```r
library(rlR)
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
agent = makeAgent("AgentActorCritic", env)
```

```
## parameters: 
## -render: - TRUE-
## -agent.gamma: - 0.99-
## -policy.maxEpsilon: - 1-
## -policy.minEpsilon: - 0.01-
## -policy.decay: - 0.999000499833375-
## -replay.memname: - Latest-
## -replay.epochs: - 1-
## -interact.maxiter: - 500-
## -log: - FALSE-
## -console: - FALSE-
## -policy.name: - EpsilonGreedy-
## -agent.nn.arch.actor: nhidden- 64-
##  -agent.nn.arch.actor: act1- relu-
##  -agent.nn.arch.actor: act2- softmax-
##  -agent.nn.arch.actor: loss- categorical_crossentropy-
##  -agent.nn.arch.actor: lr- 0.025-
##  -agent.nn.arch.actor: kernel_regularizer- regularizer_l2(l=0.0001)-
##  -agent.nn.arch.actor: bias_regularizer- regularizer_l2(l=0.0001)-
##  -agent.nn.arch.actor: decay- 0.9-
##  -agent.nn.arch.actor: clipnorm- 5-
## -agent.nn.arch.critic: nhidden- 64-
##  -agent.nn.arch.critic: act1- relu-
##  -agent.nn.arch.critic: act2- linear-
##  -agent.nn.arch.critic: loss- mse-
##  -agent.nn.arch.critic: lr- 0.025-
##  -agent.nn.arch.critic: kernel_regularizer- regularizer_l2(l=0.0001)-
##  -agent.nn.arch.critic: bias_regularizer- regularizer_l2(l=0)-
##  -agent.nn.arch.critic: decay- 0.9-
##  -agent.nn.arch.critic: clipnorm- 5-
```

```r
system.time({
perf = agent$learn(500)
})
```

```
##    user  system elapsed 
## 683.860  11.168 730.674
```

```r
perf$plot()
```

```
## `geom_smooth()` using method = 'loess'
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)



