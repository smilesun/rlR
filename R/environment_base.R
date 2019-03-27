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
#'   \item{evaluateArm(vec_arm)}{[\code{function}] \cr
#'   process value of vec_arm which is the same length vector as action count act_cnt to only generate legal action, by default doing nothing}
#' }
#' @return [\code{\link{Environment}}].
#' @export
Environment = R6::R6Class("Environment",
  public = list(
    act_cnt = NULL,
    state_dim = NULL,
    name = NULL,
    flag_continous = FALSE,
    flag_tensor = FALSE,
    observ_stack_len = 1L,
    maxStepPerEpisode = 1e4L,
    agent = NULL,  # used to get access to replaymem
    initialize = function() {
    },

    evaluateArm = function(vec_arm) {
      return(vec_arm)
    },

    # environment get a hook to agent so it can access the replay memory
    setAgent = function(agent) {
      self$agent = agent
      self$agent$mem$observ_stack_len = self$observ_stack_len
    },

    render = function() {

    },

    overview = function() {
      cat(sprintf("\naction cnt: %s \n", toString(self$act_cnt)))
      cat(sprintf("state dim: %s \n", toString(self$state_dim)))
      cat(sprintf("%s\n", ifelse(self$flag_continous, "continous action", "discrete action")))
    },

    reset = function() {
    },

    step = function(action) {
    },

    afterAll = function() {
    },

    print = function() {
      self$overview()
    }
  )
)

EnvToy = R6::R6Class("EnvToy",
  inherit = Environment,
  public = list(
    initialize = function(...) {
      self$act_cnt = c(2)
      self$state_dim = c(4)
    },

    reset = function() {
      return(list(
          state = array(rnorm(self$state_dim), dim = self$state_dim),
          reward = NULL,
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
    }
  )
)
