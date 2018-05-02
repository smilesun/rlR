# Major class
## Conf
Where to find Conf.R, Conf_static.R
### Cons
- Global available to the user the whole configuration to everything
- The whole configuration could be logged in an experiment run and refered to in the future for analysis
- Easier for Hyperparameter optimization
- Easier for benchmark
### Pros


### Pitfalls

- never initialize a variable in R6 directly like in java since this leads to contant non-changable configuration, for R direct parameter initialization will stop later configuration since this might be executed earlier than the reconfiguration, so the reconfiguration could not change this class field anymore 


