#' @title Reinforcement Learning Environment
#'
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link{R6Class}} to represent reinforcement learning environments. To define custom environment, one should define a \code{\link{R6Class}} which inherit rlR::Environment.
#'
#' @section Member Variables:
#'
#' \describe{
#'   \item{act_cnt}{[\code{int}] \cr
#'     Number of actions of the agent to environment
#'   }
#'   \item{state_dim}{[\code{vector(int)}] \cr
#'     The dimension of the observation(or state) space on the environment. Must be vector of integers. For example, c(28, 28, 3), which can be the dimension for a tensor of order 3.
#'     }
#'   \item{name}{[\code{character}] \cr
#'     A string to represent the name of the environment}
#'   \item{flag_continous}{[\code{logic}] \cr
#'     A boolean variable to represent whether the action space is continous or not}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(...)}{[\code{function}] \cr
#'   Constructor function to initialize environment}
#'   \item{step(action)}{[\code{function}] \cr
#'   Function to make a step in the environment. Must return a named list of [\code{state(array of size state_dim), reward(reward the agent get after making the step), done(boolean variable whether the episode is finished or not), info(list of anything)}]. There must be stoping criteria in step function which should return [\code{list(state = state, reward = reward, done = TRUE, info = list())}] to stop the interaction between the environment and the agent.}
#'   \item{reset()}{[\code{function}] \cr
#'   Reset the environment}
#'   \item{render()}{[\code{function}] \cr
#'   Print out information to user about the environment, can be left empty}
#'   \item{afterAll()}{[\code{function}] \cr
#'   What needs to be done after learning is finished, could be left empty}
#' }
#' @return [\code{\link{Environment}}].
#' @export
Environment = R6::R6Class("Environment",
  public = list(
    act_cnt = NULL,
    state_dim = NULL,
    name = NULL,
    flag_continous = NULL,
    initialize = function(...) {
      self$act_cnt = c(2)
      self$state_dim = c(4)
      self$name = "rlR.base.env"
      self$flag_continous = FALSE
    },

    render = function(...) {
      # you could leave this field empty
    },

    overview = function() {
      cat(sprintf("\naction cnt: %s \n", toString(self$act_cnt)))
      cat(sprintf("state dim: %s \n", toString(self$state_dim)))
      cat(sprintf("%s\n", ifelse(self$flag_continous, "continous action", "discrete action")))
    },

    reset = function() {
      return(list(
          state = array(rnorm(self$state_dim), dim = self$state_dim),
          reward = 1.0,
          done = FALSE,
          info = list()
      ))
    },

    step = function(action) {
      return(list(
          state = array(rnorm(self$state_dim), dim = self$state_dim),
          reward = 1.0,
          done = TRUE,
          info = list()
      ))
    },

    afterAll = function() {
      # what to do after the whole learning is finished?  could be left empty
    }
  ),
  private = list(),
  active = list()
)

testEnv = function() {
  env = rlR::Environment$new()
  env$overview()
  agent = makeAgent("AgentDQN", env)
  agent$learn(10)
}
