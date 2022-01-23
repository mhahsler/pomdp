#' Define an MDP Problem
#'
#' Defines all the elements of a MDP problem and formulates them as a POMDP
#' where all states are observable.
#'
#' The MDP is formulated as a POMDP where all states are completely observable.
#' This is achieved by defining one observation per state with identity
#' observation probability matrices.
#'
#' More details on specifying the parameters can be found in the documentation
#' for [POMDP].
#'
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
#' @param terminal_values a vector with the terminal values for each state.
#' @param start Specifies in which state the MDP starts.
#' @param max logical; is this a maximization problem (maximize reward) or a
#' minimization (minimize cost specified in `reward`)?
#' @param name a string to identify the MDP problem.
#' @param x a `MDP` object.
#' 
#' @return The function returns an object of class POMDP which is list with an
#' element called `'model'` containing a list with the model specification.
#' [solve_POMDP()] reads the object and adds a list element called
#' `'solution'`.
#' @author Michael Hahsler
#' @examples
#'
#' ## Michael's Sleepy Tiger Problem is an MDP with perfect observability
#'
#' STiger <- MDP(
#'   name = "Michael's Sleepy Tiger Problem",
#'   discount = 1,
#'
#'   states = c("tiger-left" , "tiger-right"),
#'   actions = c("open-left", "open-right", "do-nothing"),
#'   start = "tiger-left",
#'
#'   transition_prob = list(
#'     "open-left" =  "uniform",
#'     "open-right" = "uniform",
#'     "do-nothing" = "identity"),
#'
#'   # the rew helper expects: action, start.state, end.state, observation, value
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
#' STiger$model
#' 
#' # convert the MDP into a POMDP
#' STiger_POMDP <- MDP2POMDP(STiger)
#' STiger_POMDP
#' 
#' # do 5 epochs with no discounting
#' s <- solve_POMDP(STiger_POMDP, method = "enum", horizon = 5)
#' s
#'
#' # value function and policy
#' plot_value_function(s)
#' policy(s)
#' @export
MDP <- function(states,
  actions,
  transition_prob,
  reward,
  discount = .9,
  horizon = Inf,
  terminal_values = 0,
  start = "uniform",
  max = TRUE,
  name = NA) {
  
  ### unsolved pomdp model
  x <- list(
    model = list(
      name = name,
      discount = discount,
      horizon = horizon,
      states = states,
      actions = actions,
      transition_prob = transition_prob,
      reward = reward,
      start = start,
      terminal_values = terminal_values,
      max = max
    )
  )
  
  class(x) <- "MDP"
  check_and_fix_MDP(x)
}

#' @export
print.MDP <- print.POMDP

#' @rdname MDP
#' @export
MDP2POMDP <- function(x) {
  # add an observation for each state and identity observation_probability for all actions ('*') 
  # (note: pomdp-solve does not support "identity" for observation_probs)
  x$model$observations <- x$model$states
  ident_matrix <- diag(length(x$model$states))
  dimnames(ident_matrix) <- list(x$model$states, x$model$observations)
  
  x$model$observation_prob <- list('*' = ident_matrix)
  class(x) <- c("MDP", "POMDP")
  x
}
