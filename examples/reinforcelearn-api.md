---
title: "Reinforcement Learning API"
author: Markus Dumke
output:
  html_document:
    keep_md: TRUE
---






```r
# Switch to branch "markus"
devtools::load_all()
```

```
#> Loading rlR
```

```r
library(keras)
```

Environment


```r
env = GridworldEnvironment$new(shape = c(4, 4), goal.states = c(0), initial.state = 15)
```

State Preprocessor


```r
preprocessState = function(state) {
  reinforcelearn::nHot(state + 1, env$n.states)
}

(s = env$reset())
```

```
#> [1] 15
```

```r
(s = preprocessState(s))
```

```
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#> [1,]    0    0    0    0    0    0    0    0    0     0     0     0     0
#>      [,14] [,15] [,16]
#> [1,]     0     0     1
```

ActionValueNetwork


```r
model.keras = keras_model_sequential()
model.keras %>% layer_dense(units = 4, activation = "linear", input_shape = c(16),
  use_bias = FALSE, kernel_initializer = "zeros")
keras::compile(model.keras, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.4))

action.vals = ActionValueNetwork$new(model.keras, preprocessState)
action.vals$model
```

```
#> Model
#> ___________________________________________________________________________
#> Layer (type)                     Output Shape                  Param #     
#> ===========================================================================
#> dense_1 (Dense)                  (None, 4)                     64          
#> ===========================================================================
#> Total params: 64.0
#> Trainable params: 64
#> Non-trainable params: 0.0
#> ___________________________________________________________________________
```

```r
(Q = action.vals$predictQ(s))
```

```
#>      [,1] [,2] [,3] [,4]
#> [1,]    0    0    0    0
```

```r
# action.vals$train(state, target)
```

Policy


```r
policy = EpsilonGreedyPolicy$new(epsilon = 0.1)
policy$epsilon
```

```
#> [1] 0.1
```

```r
(probs = policy$getActionProbs(Q))
```

```
#>       [,1]  [,2]  [,3]  [,4]
#> [1,] 0.925 0.025 0.025 0.025
```

```r
(action = policy$sampleAction(probs))
```

```
#> [1] 3
```

Learner


```r
learner = QLearning$new()
```

Agent


```r
agent = Agent$new(learner, action.vals, policy)
```

Interaction


```r
interaction(env, agent, n.steps = 200)
```

```
#> Episode 1 finished after 28 steps with a return of -28
```

```
#> Episode 2 finished after 31 steps with a return of -31
```

```
#> Episode 3 finished after 20 steps with a return of -20
```

```
#> Episode 4 finished after 19 steps with a return of -19
```

```
#> Episode 5 finished after 30 steps with a return of -30
```

```
#> Episode 6 finished after 20 steps with a return of -20
```

```
#> Episode 7 finished after 9 steps with a return of -9
```

```
#> Episode 8 finished after 18 steps with a return of -18
```

```
#> Episode 9 finished after 13 steps with a return of -13
```

Get Action Value Function


```r
agent$action.value$model %>% get_weights()
```

```
#> [[1]]
#>            [,1]      [,2]      [,3]      [,4]
#>  [1,]  0.000000  0.000000  0.000000  0.000000
#>  [2,] -1.000000 -1.000000 -2.000000 -1.000000
#>  [3,] -2.000000 -2.000000 -2.000000 -2.000000
#>  [4,] -2.999999 -2.000000 -2.000000 -2.000000
#>  [5,] -2.000000 -2.000000 -1.000000 -1.000000
#>  [6,] -2.000000 -2.000000 -2.000000 -2.000000
#>  [7,] -2.999999 -2.999999 -2.000000 -2.000000
#>  [8,] -2.999999 -2.999999 -2.999999 -2.000000
#>  [9,] -2.999999 -2.999999 -2.000000 -2.000000
#> [10,] -2.999999 -2.999999 -2.999999 -2.000000
#> [11,] -2.999999 -2.999999 -2.999999 -2.999999
#> [12,] -2.999999 -2.999999 -2.999999 -2.999999
#> [13,] -3.999998 -3.999998 -2.999999 -2.999999
#> [14,] -3.999998 -2.999999 -2.999999 -2.999999
#> [15,] -2.999999 -2.999999 -2.999999 -2.999999
#> [16,] -3.999998 -3.999998 -3.999998 -2.999999
```


---
title: "reinforcelearn-api.R"
author: "M"
date: "Mon Dec 04 11:18:53 2017"
---
