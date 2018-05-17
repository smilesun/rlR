- Wrong Data processing
x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage, bug point is it changes the orientation of the replay memory, but works for mountain car, with batchsize 5
y = array(unlist(list.targets), dim = c(length(list.targets), self$actCnt))
- Wrong action arm to be updated
- boundary condition: instance at end of episode
- decay every step
- reduce redundant computation
- promise already under evaluation: recursive default argument reference or earlier problems: same
  name is used two times in arguments passing:
    mkInst = function(state.old, action, reward, state.new, done, info = info) {
      list(state.old = state.old, action = action, reward = reward, state.new = state.new, done = done, info = info)
    },

 
