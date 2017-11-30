gridworld = function(shape = NULL, goal.states = NULL, cliff.states = NULL,
  reward.step = -1, reward.cliff = -100, diagonal.moves = FALSE, wind = rep(0, shape[2]),
  cliff.transition.states = NULL, cliff.transition.done = FALSE, stochasticity = 0, ...) {

  checkmate::assertIntegerish(shape, len = 2)
  if (prod(shape) <= 1) {
    stop("A gridworld with only one state is not allowed!")
  }
  checkmate::assertIntegerish(goal.states)
  goal.states = goal.states + 1
  checkmate::assertIntegerish(cliff.states, null.ok = TRUE)
  if (!is.null(cliff.states)) {
    cliff.states = cliff.states + 1
  }
  checkmate::assertIntegerish(cliff.transition.states, null.ok = TRUE)
  if (!is.null(cliff.transition.states)) {
    cliff.transition.states = cliff.transition.states + 1
  }
  if (any(goal.states > prod(shape)) || any(cliff.states > prod(shape)) |
      any(cliff.transition.states > prod(shape))) {
    stop("All states must be inside the grid! States are numerated row-wise starting with 0, check Details!")
  }
  checkmate::assertIntegerish(wind, len = shape[2])
  checkmate::assertNumber(reward.step)
  checkmate::assertNumber(reward.cliff)
  checkmate::assertFlag(diagonal.moves)
  checkmate::assertFlag(cliff.transition.done)
  checkmate::assertNumber(stochasticity, lower = 0, upper = 1)

  n.states = prod(shape)
  states = seq_len(n.states)
  n.col = shape[2]
  if (diagonal.moves) {
    n.actions = 8
  } else {
    n.actions = 4
  }

  rewards = makeRewardMatrix(reward.step, reward.cliff, n.states, n.actions,
    cliff.states, goal.states)

  transitions = array(matrix(0, nrow = n.states, ncol = n.states),
    dim = c(n.states, n.states, 8))

  border.states = list(left = seq(1, n.states - n.col + 1, n.col),
    right = seq(n.col, n.states, n.col),
    up = seq(1, n.col),
    down = seq(n.states - n.col + 1, n.states))

  non.terminal.states = setdiff(states, c(goal.states, cliff.states))
  actions = list("left", "right", "up", "down", "leftup", "leftdown", "rightup", "rightdown")
  actions = lapply(actions, function(x) {class(x) = x; x})

  m.cliff = NULL
  if (cliff.transition.done) {
    goal.states = c(goal.states, cliff.states)
  } else {
    if (!is.null(cliff.states)) {
      if (!is.null(cliff.transition.states)) {
        cliff.pairs = as.matrix(expand.grid(cliff.states, cliff.transition.states))
        cliff.prob = 1 / length(cliff.transition.states)
        m.cliff = cbind(cliff.pairs, cliff.prob)
      } else {
        non.terminal.states = c(non.terminal.states, cliff.states)
      }
    }
  }

  n.states = length(non.terminal.states)
  new.states = vapply(actions, go, states = non.terminal.states, border.states = border.states,
    n.col = n.col, FUN.VALUE = numeric(n.states))

  if (!is.matrix(new.states)) {
    new.states = as.matrix(new.states, nrow = 1)
  }

  m.stoch = matrix(0, nrow = n.states * 8, ncol = 3)
  m.stoch[, 1] = rep(non.terminal.states, 8)
  m.stoch[, 2] = c(new.states)
  m.stoch[, 3] = stochasticity / 8

  m.goal = matrix(c(goal.states, goal.states, rep(1, length(goal.states))), ncol = 3)
  m = rbind(m.cliff, m.goal, m.stoch)
  m = m[rep(seq_len(nrow(m)), each = 8), ]
  m = cbind(m, action = rep(1:8, nrow(m) / 8))

  new.states = c(apply(new.states, 2, applyWind, states = non.terminal.states, n.col = n.col, wind = wind))
  new.states = getIntoBounds(new.states, n.col = n.col)

  m2 = matrix(c(rep(non.terminal.states, 8), new.states, rep(1 - stochasticity, length(new.states)),
    rep(1:8, each = length(non.terminal.states))), ncol = 4)
  m = rbind(m, m2)
  colnames(m) = c("row", "col", "prob", "action")

  m = as.matrix(aggregate(prob ~ row + col + action, data = as.data.frame(m), FUN = "sum"))
  transitions[m[, c("row", "col", "action")]] = m[, "prob"]
  transitions = transitions[, , seq_len(n.actions)]

  visualize = function(env) {
    one.row = paste(rep("-", shape[2]), collapse = " ")
    grid.vis = paste("", one.row, collapse = "")
    for (i in seq_len(shape[1] - 1)) {
      grid.vis = paste(grid.vis, "\n", one.row)
    }

    n = env$state + 1
    # find position of nth -
    str.pos = gregexpr("-", grid.vis)[[1]][n]
    # replace nth - with o (current state in grid)
    grid.vis = sub(paste0("^(.{", str.pos - 1, "})(.)(.*$)", collapse = ""),
      "\\1o\\3", grid.vis)
    cat(grid.vis)
    ## some unsuccessful tries for the same thing
    # str_replace(grid.vis, "(.-[^-]*){7}", "\\o")
    # sub("^((?:[^-]*-){2}).*", "\\o", grid.vis)
    # pat <- paste0("^((?:.*?-){", n - 1, "}.*?)-")
    # pat <- paste0("(.?-.?){", n - 1, "}")
    # sub("(.?-.?)", " o ", grid.vis)#, perl = TRUE)
  }
  makeEnvironment(transitions = transitions, rewards = rewards, visualize = visualize, ...)
}

makeRewardMatrix = function(reward.step, reward.cliff, n.states, n.actions,
  cliff.states, goal.states) {
  rewards = matrix(reward.step, nrow = n.states, ncol = n.actions)
  rewards[cliff.states, ] = reward.cliff
  rewards[goal.states, ] = 0
  rewards
}

go = function(x, ...) {
  UseMethod("go", x)
}

#' @export
go.left = function(x, states, border.states, ...) {
  ifelse(states %in% border.states[["left"]], states, states - 1)
}

#' @export
go.right = function(x, states, border.states, ...) {
  ifelse(states %in% border.states[["right"]], states, states + 1)
}

#' @export
go.up = function(x, states, border.states, n.col) {
  ifelse(states %in% border.states[["up"]], states, states - n.col)
}

#' @export
go.down = function(x, states, border.states, n.col) {
  ifelse(states %in% border.states[["down"]], states, states + n.col)
}

#' @export
go.leftup = function(x, states, border.states, n.col) {
  go.left(x, go.up(x, states, border.states, n.col), border.states)
}

#' @export
go.leftdown = function(x, states, border.states, n.col) {
  go.left(x, go.down(x, states, border.states, n.col), border.states)
}

#' @export
go.rightup = function(x, states, border.states, n.col) {
  go.right(x, go.up(x, states, border.states, n.col), border.states)
}

#' @export
go.rightdown = function(x, states, border.states, n.col) {
  go.right(x, go.down(x, states, border.states, n.col), border.states)
}

applyWind = function(states, new.states, wind, n.col) {
  column = states %% n.col
  column[column == 0] = n.col
  new.states - wind[column] * n.col
}

getIntoBounds = function(new.states, n.col) {
  while (any(new.states <= 0)) {
    new.states[new.states <= 0] = new.states[new.states <= 0] + n.col
  }
  new.states
}
