- Wrong Data processing
x = array(unlist(list.states), dim = c(length(list.states), dim(list.states[[1L]])))  # matrix will make row wise storage, bug point is it changes the orientation of the replay memory, but works for mountain car, with batchsize 5
y = array(unlist(list.targets), dim = c(length(list.targets), self$actCnt))
- Wrong action arm to be updated
