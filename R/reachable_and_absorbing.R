#' Reachable and Absorbing States
#'
#' Find reachable and absorbing states in the transition model.
#'
#' The function `reachable_states()` checks if states
#' are reachable using the transition model.
#'
#' The function `absorbing_states()` checks if a state or a set of states are
#' absorbing (terminal states) with a zero reward (or `-Inf` for unavailable actions).
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
        transition_matrix(
          x,
          action = a,
          start.state = s,
          end.state = s,
          drop = TRUE
        )
    ) == 1)
    # &&
    #   all(sapply(
    #     x$actions,
    #     FUN = function(a) {
    #       r <- reward_matrix(x,
    #                       action = a,
    #                       start.state = s,
    #                       end.state = s)
    #       all(r == 0 | r == -Inf)
    #     }
    #   ))
    )
  
  
  if (is.null(states))
    states <- x$states
  
  if (is.numeric(states))
    states <- x$states[states]
  
  structure(sapply(states,
                   is_absorbing,
                   x), names = states)
}

#' @rdname reachable_and_absorbing
#' @returns the model with all unreachable states removed
#' @export
remove_unreachable_states <- function(x) {
  reachable <- reachable_states(x)
  if (all(reachable))
    return(x)

  keep_names <- x$states[reachable]

  keep_data_frame_states <- function(field, columns) {
    columns <- intersect(columns, names(field))
    keep <- rep(TRUE, nrow(field))
    for (column in columns) {
      values <- as.character(field[[column]])
      keep <- keep & (is.na(field[[column]]) | values %in% keep_names)
    }
    field <- field[keep, , drop = FALSE]
    for (column in columns) {
      if (is.factor(field[[column]]))
        field[[column]] <- factor(as.character(field[[column]]), levels = keep_names)
    }
    field
  }

  keep_transition_states <- function(field) {
    if (is.data.frame(field)) {
      return(keep_data_frame_states(field, c("start.state", "end.state")))
    }
    if (is.function(field))
      return(field)

    lapply(field, function(matrix_or_keyword) {
      if (is.character(matrix_or_keyword))
        return(matrix_or_keyword)
      matrix_or_keyword[reachable, reachable, drop = FALSE]
    })
  }

  keep_observation_states <- function(field) {
    if (is.data.frame(field))
      return(keep_data_frame_states(field, "end.state"))
    if (is.function(field))
      return(field)

    lapply(field, function(matrix_or_keyword) {
      if (is.character(matrix_or_keyword))
        return(matrix_or_keyword)
      matrix_or_keyword[reachable, , drop = FALSE]
    })
  }

  keep_reward_states <- function(field) {
    if (is.data.frame(field))
      return(keep_data_frame_states(field, c("start.state", "end.state")))
    if (is.function(field))
      return(field)

    lapply(field, function(reward_by_start_state) {
      reward_by_start_state <- reward_by_start_state[reachable]
      lapply(reward_by_start_state, function(reward_matrix) {
        reward_matrix[reachable, , drop = FALSE]
      })
    })
  }

  keep_field_states <- function(field, field_name, filter) {
    if (.is_timedependent_field(x, field_name))
      lapply(field, filter)
    else
      filter(field)
  }
  
  # fix start state
  if (is.numeric(x$start)) {
    if (length(x$start) == length(x$states)) {
      ### prob vector
      x$start <- x$start[reachable]
      if (sum(x$start) != 1)
        stop(
          "Probabilities for reachable states do not sum up to one! An unreachable state had a non-zero probability."
        )
    } else
      ### state ids... we translate to state names
      x$start <- x$states[x$start]
  }
  if (is.character(x$start)) {
    if (identical(x$start, "uniform")) {
      # do nothing
    } else {
      x$start <- intersect(x$start, keep_names)
    }
    if (length(x$start) == 0L)
      stop("Start state is not reachable.")
  }
  
  x$transition_prob <- keep_field_states(
    x$transition_prob,
    "transition_prob",
    keep_transition_states
  )
  x$reward <- keep_field_states(x$reward, "reward", keep_reward_states)
  if (!is.null(x$observation_prob))
    x$observation_prob <- keep_field_states(
      x$observation_prob,
      "observation_prob",
      keep_observation_states
    )

  if (!is.null(x$terminal_values) && length(x$terminal_values) > 1L) {
    if (is.matrix(x$terminal_values))
      x$terminal_values <- x$terminal_values[, reachable, drop = FALSE]
    else
      x$terminal_values <- x$terminal_values[reachable]
  }

  if (!is.null(x$solution)) {
    if (inherits(x, "POMDP")) {
      x$solution$alpha <- lapply(
        x$solution$alpha,
        function(alpha) alpha[, reachable, drop = FALSE]
      )
      if (!is.null(x$solution$belief_points_solver))
        x$solution$belief_points_solver <-
          x$solution$belief_points_solver[, reachable, drop = FALSE]
      if (!is.null(x$solution$initial_belief))
        x$solution$initial_belief <- x$solution$initial_belief[reachable]
      if (!is.null(x$solution$central_belief))
        x$solution$central_belief <- lapply(
          x$solution$central_belief,
          function(belief) belief[, reachable, drop = FALSE]
        )
    } else if (!is.null(x$solution$policy)) {
      x$solution$policy <- lapply(x$solution$policy, function(policy) {
        policy <- policy[as.character(policy$state) %in% keep_names, , drop = FALSE]
        if (is.factor(policy$state))
          policy$state <- factor(as.character(policy$state), levels = keep_names)
        policy
      })
    }
  }

  x$states <- keep_names
  
  # just check
  check_and_fix_MDP(x)
  x
}
