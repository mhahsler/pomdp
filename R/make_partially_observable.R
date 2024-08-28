#' Convert between MDPs and POMDPs
#'
#' Convert a MDP into POMDP by adding an observation model or
#' a POMDP into a MDP by making the states observable.
#'
#' `make_partially_observable()` adds an observation model to an MDP. If no observations and
#' observation probabilities are provided, then an observation for each state is created
#' with identity observation matrices. This means we have a fully observable model
#' encoded as a POMDP.
#'
#' `make_fully_observable()` removes the observation model from a POMDP and returns
#' an MDP.
#'
#' @family POMDP
#'
#' @name MDP2POMDP
#'
#' @param x a `MDP` or a `POMDP` object.
#' @param observations a character vector specifying the names of the available
#' observations.
#' @param observation_prob Specifies the observation probabilities (see [POMDP] for details).
#' @param x a `MDP` object.
#'
#' @returns a `MDP` or a `POMDP` object.
#' @author Michael Hahsler
#' @examples
#' # Turn the Maze MDP into a partially observable problem.
#' # Here each state has an observation, so it is still a fully observable problem
#' # encoded as a POMDP.
#' data("Maze")
#' Maze
#'
#' Maze_POMDP <- make_partially_observable(Maze)
#' Maze_POMDP
#'
#' sol <- solve_POMDP(Maze_POMDP)
#' policy(sol)
#' simulate_POMDP(sol, n = 1, horizon = 100, return_trajectories = TRUE)$trajectories
#'
#' # Make the Tiger POMDP fully observable
#' data("Tiger")
#' Tiger
#'
#' Tiger_MDP <- make_fully_observable(Tiger)
#' Tiger_MDP
#'
#' sol <- solve_MDP(Tiger_MDP)
#' policy(sol)
#' # The result is not exciting since we can observe where the tiger is!
#' @export
make_partially_observable <- function(x,
                                      observations = NULL,
                                      observation_prob = NULL) {
  if (!inherits(x, "MDP"))
    stop("'x' needs to be of class 'MDP'.")
  
  if (is.null(observations))
    observations <- x$states
  x$observations <- observations
  
  if (is.null(observation_prob)) {
    ident_matrix <- diag(length(x$states))
    dimnames(ident_matrix) <- list(x$states, x$observations)
    
    observation_prob <-
      sapply(
        x$actions,
        FUN = function(x)
          ident_matrix,
        simplify = FALSE
      )
  }
  
  x$observation_prob <- observation_prob
  
  # add missing observations to reward data.frame
  if (is.data.frame(x$reward))
    x$reward <- data.frame(
      action = x$reward$action,
      start.state = x$reward$start.state,
      end.state = x$reward$end.state,
      observation = factor(NA_character_, levels = x$states),
      value = x$reward$value
    )
  else
    stop("Reward needs to be a data.frame!")
  
  class(x) <- c("POMDP", "list")
  check_and_fix_MDP(x)
}

#' @rdname MDP2POMDP
#' @export
make_fully_observable <- function(x) {
  if (!inherits(x, "POMDP"))
    stop("'x' needs to be of class 'POMDP'.")
  
  x$observations <- NULL
  x$observation_prob <- NULL
  
  # remove observations from reward data.frame
  if (is.data.frame(x$reward))
    x$reward$observation <- NULL
  else
    stop("Reward needs to be a data.frame!")
  
  class(x) <- c("MDP", "list")
  check_and_fix_MDP(x)
}