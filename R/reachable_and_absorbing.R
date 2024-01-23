#' Reachable and Absorbing States
#'
#' Find reachable and absorbing states in the transition model.
#'
#' The function `reachable_states()` checks if states
#' are reachable using the transition model.
#'
#' The function `absorbing_states()` checks if a state or a set of states are
#' absorbing (terminal states) with a reward of zero.
#' If no states are specified (`states = NULL`), then all model states are
#' checked. This information can be used in simulations to end an episode.
#'
#' The function `remove_unreachable_states()` simplifies a model by
#' removing unreachable states.
#' @name reachable_and_absorbing 
#' @aliases reachable_and_absorbing 
#' @family MDP
#' @family POMDP
#'
#' @param x a `MDP` pr `POMDP` object.
#' @param states a character vector specifying the names of the states to be 
#'  checked. `NULL` checks all states. 
#'
#' @author Michael Hahsler
#' @examples
#' data(Maze)
#' 
#' gridworld_matrix(Maze, what = "label")
#'
#' # the states marked with +1 and -1 are absorbing
#' absorbing_states(Maze)
#' which(absorbing_states(Maze))
#' 
#' # all states in the model are reachable
#' reachable_states(Maze)
#' which(!reachable_states(Maze))
#' 
#' @returns  `reachable_states()` returns a logical vector indicating
#'    if the states are reachable.
#' @importFrom Matrix colSums
#' @export
reachable_states <- function(x,
                             states = NULL) {
  r <- Reduce("+", transition_matrix(x))
  diag(r) <- 0
  if (!is.null(states))
    r <- r[, states, drop = FALSE]
  colSums(r) > 0
}
  
#' @rdname reachable_and_absorbing
#' @returns  `absorbing_states()` returns a logical vector indicating
#'    if the states are absorbing (terminal).
#' @export
absorbing_states <- function(x,
                             states = NULL) {
  is_absorbing <- function(s, x)
    (all(sapply(
      x$actions,
      FUN = function(a)
        transition_val(
          x,
          action = a,
          start.state = s,
          end.state = s
        )
    ) == 1) &&
      all(sapply(
        x$actions,
        FUN = function(a)
          reward_val(
            x,
            action = a,
            start.state = s,
            end.state = s
          )
      ) == 0))
  
  if (is.null(states))
    states <- x$states
  
  if (is.numeric(states))
    states <- x$states[states]
  
  structure(sapply(
    states,
    is_absorbing,
    x
  ), names = states)
}

#' @rdname reachable_and_absorbing
#' @returns the model with all unreachable states removed
#' @export
remove_unreachable_states <- function(x) {
  reachable <- reachable_states(x)
  if(all(reachable)) 
    return(x)
  
  keep_states <- function(field, states) {
    if (is.data.frame(field)) {
      keep_names <- names(which(states))
      field <-
        field[field$start.state %in% c(NA, keep_names) &
                field$end.state %in% c(NA, keep_names), , drop = FALSE]
      field$start.state <- factor(as.character(field$start.state), levels = keep_names)
      field$end.state <- factor(as.character(field$end.state), levels = keep_names)
    } else {
      ### a list of actions
      field <-
        lapply(
          field,
          FUN = function(m) {
            if (!is.character(m))
              ### strings like "uniform"
              m <- m[states, states, drop = FALSE]
            m
          }
        )
    }
    field
  }
  
  # fix start state
  if (is.numeric(x$start)) { 
    if (length(x$start) == length(x$states)) { ### prob vector
      x$start <- x$start[reachable]
      if (sum(x$start) == 1)
        stop("Probabilities for reachable states do not sum up to one! An unreachable state had a non-zero probability.")
    } else ### state ids... we translate to state names
      x$start <- x$states[x$start] 
  }
  if (is.character(x$start)) {
    if(x$start == "uniform") {
      # do nothing
    } else {
      x$start <- intersect(x$start, x$states[reachable])
    }
    if (length(x$start) == 0L)
      stop("Start state is not reachable.")
  }
    
  x$states <- x$states[reachable]
  x$transition_prob <- keep_states(x$transition_prob, reachable)
  x$reward <- keep_states(x$reward, reachable)
  if(!is.null(x$observations)) 
    x$observations <- keep_states(x$observations, reachable)
  
  # just check
  check_and_fix_MDP(x)
  x 
}

