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
#' for \code{\link{POMDP}}.
#'
#' @param states a character vector specifying the names of the states.
#' @param actions a character vector specifying the names of the available
#' actions.
#' @param transition_prob Specifies the transition probabilities between
#' states.
#' @param reward Specifies the rewards dependent on action, states and
#' observations.
#' @param discount numeric; discount rate between 0 and 1.
#' @param horizon numeric; Number of epochs. \code{Inf} specifies an infinite
#' horizon.
#' @param terminal_values a vector with the terminal values for each state.
#' @param start Specifies in which state the MDP starts.
#' @param max logical; is this a maximization problem (maximize reward) or a
#' minimization (minimize cost specified in \code{reward})?
#' @param name a string to identify the MDP problem.
#' @return The function returns an object of class POMDP which is list with an
#' element called \code{model} containing a list with the model specification.
#' \code{solve_POMDP} reads the object and adds a list element called
#' \code{solution}.
#' @author Michael Hahsler
#' @seealso \code{\link{POMDP}}, \code{\link{solve_POMDP}}.
#' @examples
#'
#' ## Michael's Sleepy Tiger Problem is an MDP with perfect observability
#'
#' Tiger_MDP <- MDP(
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
#' Tiger_MDP
#'
#' # do 5 epochs with no discounting
#' s <- solve_POMDP(Tiger_MDP, method = "enum", horizon = 5)
#' s
#'
#' # value function and policy
#' plot_value_function(s)
#' policy(s)
#'
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
  ### FIXME: Check the values!
  
  # discount should be a number in [0,1]
  # states should be a vector of strings
  # actions should be a vector of strings
  # observations should be a vector of strings
  # start should be either a vector of n numbers each in [0,1] that add up to 1 where n is the number of states
  # or the word "uniform", or a single number in 1 to n, or the name of a single state, or the names of a subset of states
  
  ### add names to start
  if (is.numeric(start) && length(start) == length(states)) {
    if (is.null(names(start)))
      names(start) <- states
    else
      start <- start[states]
  }
  
  # transition_prob is either a list consisting of m matrices where m is the number of actions
  # or a data frame with 4 columns
  
  ### add names to transition probabilities
  for (a in names(transition_prob)) {
    if (is.matrix(transition_prob[[a]]))
      dimnames(transition_prob[[a]]) <- list(states, states)
  }
  
  # Define a MDP using a POMDP
  observations <- states
  identity_matrix <-
    diag(1, nrow = length(observations), ncol = length(states))
  observation_prob <-
    sapply(
      actions,
      FUN = function(a)
        identity_matrix,
      simplify = FALSE
    )
  
  # observation_prob is either a list consisting of m matrices where m is the number of actions
  # or a data frame with 4 columns
  # reward should be either a matrix of size mxn where n is the number of states or
  # a data frame with 5 columns
  # grid_size is an integer
  
  ### unsolved pomdp model
  structure(list(model = structure(
    list(
      name = name,
      discount = discount,
      horizon = horizon,
      states = factor(states),
      actions = factor(actions),
      observations = factor(observations),
      transition_prob = transition_prob,
      observation_prob = observation_prob,
      reward = reward,
      start = start,
      terminal_values = terminal_values,
      max = max
    ),
    class = "POMDP_model"
  )), class = c("MDP", "POMDP"))
}

#' @export
print.MDP <- function(x, ...) {
  if (is.null(x$solution))
    cat("Unsolved MDP model (formulated as a POMDP):",
      x$model$name,
      "\n")
  else
    cat(
      "Solved MDP model (formulated as a POMDP):",
      x$model$name,
      "\n",
      "\tsolution method:",
      x$solution$method,
      "\n",
      "\thorizon:",
      x$solution$horizon,
      paste0("(converged: ", x$solution$converged, ")"),
      "\n",
      "\ttotal expected reward (for start probabilities):",
      x$solution$total_expected_reward,
      "\n"
    )
  cat("\n")
}
