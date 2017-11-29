env = Environment(gym.name = "MountainCar-v0")

library(keras)
keras.model = Sequential() %>% addDense()

val.fun = ActionValueNetwork(keras.model)
# val.fun = ActionValueTable()
# predict, fit

policy = Policy("e-greedy", epsilon = 0.1)
# sampleAction

learner = Learner("sarsa")
# learn

agent = Agent(learner, val.fun, policy)

experiment.results = Experiment(environment, agent, n.episodes = 100)
# logging etc.
# step modus