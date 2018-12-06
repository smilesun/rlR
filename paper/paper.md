---
title: 'rlR: A R package for deep reinforcement learning'
tags:
- R
- reinforcement learning
- deep learning
authors:
- name: Xudong Sun
orcid: 0000-0001-9234-4932
affiliation: 1
- name: Sebastian Gruber
orcid: 0000-0002-8544-3470
affiliation: 1
- name: Markus Dumke
orcid: 0000-0000-0000-0000
affiliation: 1
- name: Bernd Bischl
orcid: 0000-0000-0000-0000
affiliation: 1
affiliations:
- name: Ludwig-Maximillians-University of Munich
index: 1
date: 15 October 2018
bibliography: paper.bib
output: pdf_document
---

# Summary

Deep reinforcement learning has gained increasing attention in recent years due to its success in solving
many complex scenarios including Atari Games [@Mnih2015], Continuous Robotic Control [@Lillicrap2016a],
The game of Go [@Silver2016a] and so on. Although during our package development, we noticed some light-weight R packages occurs in between for doing reinforcement learning, most of them either only have tabular learning algorithm [@Nicolas2018], or lacks the ability to handle complicated state input like image series state input(Atari games for example) or contain only a single deep reinforcement learning algorithm [@Dumke2018]. More over, as a software package, it is not only important to show examples, but should also handle user defined environments at full fledge as input and the architecture design of the package should be loose coupling as possible to incorporate new algorithms.

The package rlR aims at solving the drawbacks above by serving as a generic deep reinforcement learning solver where we expect the user to create their customized environment or scenario as input. Several deep reinforcement learning algorithms are included and examples of how to use the library are well documented. We also wrapped around the OpenAI Gym Environments including the Atari Games so the user could play with it. Tensorflow is used as our deep learning backend serving as an universal function approximator.

# Highlights

The package rlR is written in an Aspect Oriented Programming fashion which allows customized
operation during the interaction between the agent and the environment.  The package is also designed in an Object Oriented fashion with various design patterns used in software engineering which makes it easily extensible to new algorithms.

Most of the operations are configurable through a single configuration object where the user could easily
query the  meaning of each configuration parameter instead of giving different arguments to
different functions. This could greatly facilitate the reproducibility.

User could define an environment to be a R6 Class which greatly heaves the expressibility of the
customized environment. For example, The user could define the initialization for the
environment, what to do after each step and each episode, etc.

# Example
```
env = makeGymEnv("CartPole-v1")
env$overview()
conf = getDefaultConf("AgentDQN")
conf$show()
conf$set(render = FALSE, console = FALSE)
agent = makeAgent("AgentDQN", env, conf)
agent$learn(200L)  
agent$plotPerf()
```
![CartPole Scenario Performance](figures/mplot-1.png)

# Acknowledgements

We acknowledge helpful suggestions from Janek Thomas and support of DFG.

# References
