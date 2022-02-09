#' Define an MDP Problem
#'
#' Defines all the elements of a MDP problem.
#' 
#' MDPs are similar to POMDPs, however, states are completely observable and
#' observations are not necessary. The model is defined similar to [POMDP]
#' models, but observations are not specified and the `'observations'` column in
#' the the reward specification is always `'*'`.
#' 
#' `MDP2POMDP()` reformulates a MDP as a POMDP with one observation per state
#' that reveals the current state. This is achieved by defining identity
#' observation probability matrices.
#'
#' More details on specifying the model components can be found in the documentation
#' for [POMDP].
#' @include POMDP.R
#' @param states a character vector specifying the names of the states.
#' @param actions a character vector specifying the names of the available
#' actions.
#' @param transition_prob Specifies the transition probabilities between
#' states.
#' @param reward Specifies the rewards dependent on action, states and
#' observations.
#' @param discount numeric; discount rate between 0 and 1.
#' @param horizon numeric; Number of epochs. `Inf` specifies an infinite
#' horizon.
#' @param start Specifies in which state the MDP starts.
#' @param name a string to identify the MDP problem.
#' @param x a `MDP` object.
#' 
#' @return The function returns an object of class MDP which is list with 
#'   the model specification. [solve_MDP()] reads the object and adds a list element called
#' `'solution'`.
#' @author Michael Hahsler
#' @examples
#' # Michael's Sleepy Tiger Problem is like the POMDP Tiger problem, but
#' # has completely observable states because the tiger is sleeping in front
#' # of the door. This makes the problem an MDP.
#'
#' STiger <- MDP(
#'   name = "Michael's Sleepy Tiger Problem",
#'   discount = .9,
#'
#'   states = c("tiger-left" , "tiger-right"),
#'   actions = c("open-left", "open-right", "do-nothing"),
#'   start = "uniform",
#'
#'   # opening a door resets the problem
#'   transition_prob = list(
#'     "open-left" =  "uniform",
#'     "open-right" = "uniform",
#'     "do-nothing" = "identity"),
#'
#'   # the reward helper R_() expects: action, start.state, end.state, observation, value
#'   reward = rbind(
#'     R_("open-left",  "tiger-left",  v = -100),
#'     R_("open-left",  "tiger-right", v =   10),
#'     R_("open-right", "tiger-left",  v =   10),
#'     R_("open-right", "tiger-right", v = -100),
#'     R_("do-nothing",                v =    0)
#'   )
#' )
#'
#' STiger
#'
#' sol <- solve_MDP(STiger, eps = 1e-7)
#' sol
#' 
#' policy(sol)
#' plot_value_function(sol)
#' 
#' # convert the MDP into a POMDP and solve
#' STiger_POMDP <- MDP2POMDP(STiger)
#' sol2 <- solve_POMDP(STiger_POMDP)
#' sol2 
#' 
#' policy(sol2)
#' plot_value_function(sol2)
#' @export
MDP <- function(states,
  actions,
  transition_prob,
  reward,
  discount = .9,
  horizon = Inf,
  start = "uniform",
  name = NA) {
  
  ### unsolved pomdp model
  x <- list(
      name = name,
      discount = discount,
      horizon = horizon,
      states = states,
      actions = actions,
      transition_prob = transition_prob,
      reward = reward,
      start = start
  )
  
  class(x) <- list("MDP", "list")
  check_and_fix_MDP(x)
}

#' @export
print.MDP <- print.POMDP

#' @rdname MDP
#' @export
MDP2POMDP <- function(x) {
  if (!inherits(x, "MDP"))
    stop("'x' needs to be of class 'MDP'.")
  
  # add an observation for each state and identity observation_probability for all actions ('*') 
  # (note: pomdp-solve does not support "identity" for observation_probs)
  x$observations <- x$states
  ident_matrix <- diag(length(x$states))
  dimnames(ident_matrix) <- list(x$states, x$observations)
  
  x$observation_prob <- list('*' = ident_matrix)
  class(x) <- c("MDP", "POMDP", "list")
  x
}

.solved_MDP <- function(x) {
  if (!inherits(x, "MDP"))
    stop("x needs to be a POMDP object!")
  if (is.null(x$solution))
    stop("x needs to be a solved MDP. Use solve_MDP() first.")
}