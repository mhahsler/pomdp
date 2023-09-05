#' Defining a Belief Space Projection
#'
#' High dimensional belief spaces can be projected to lower dimension. This is useful for visualization and 
#' to analyze the belief space and value functions. This definition is used by functions like [plot_belief_space()],
#' [plot_value_function()], and [sample_belief_space()].
#'
#' The belief space is $n-1$ dimensional, were $n$ is the number of states. Note: it is n-1 dimensional since the probabilities 
#' need to add up to 1. A projection fixes the belief value for a set of states. For example, for a 4-state POMDP
#' (s1, s2, s3, s4), we can project the belief space on s1 and s2 by holding s3 and s4 constant
#' which is represented by the vector `c(s1 = NA, s2 = NA, s3 = 0, s4 = .1)`. We use `NA` to represent that the values are not 
#' fixed and the value that the other dimensions are held constant.
#' 
#' We provide several ways to specify a projection:
#' 
#' * A vector with values for all dimensions. `NA`s are used for the dimension projected on. This is the canonical form 
#' used in this package. Example: `c(NA, NA, 0, .1)`
#' * A named vector with just the dimensions held constant. Example: `c(s3 = 0, s4 = .1)`
#' * A vector of state names to project on. All other dimensions are held constant at 0. Example: `c("s1", "s2")`
#' * A vector with indices of the states to project on. All other dimensions are held constant at 0. Example: `c(1, 2)`
#' 
#' @family policy
#' @family POMDP
#'
#' @param x specification of the projection (see Details section).
#' @param model a [POMDP].
#' @returns a canonical description of the projection.
#' 
#' @author Michael Hahsler
#' @examples 
#' model <- POMDP(
#'  states = 4,
#'  actions = 2,
#'  observations = 2,
#'  transition_prob = list("identity","identity"),
#'  observation_prob = list("uniform","uniform"),
#'  reward = rbind(R_(value = 1))
#' )
#'
#' projection(NULL, model = model)
#' projection(1:2, model = model)
#' projection(c("s2", "s3"), model = model)
#' projection(c(1,4), model = model)
#' projection(c(s2 = .4, s3 = .2), model = model)
#' projection(c(s1 = .1, s2 = NA, s3 = NA, s4 = .3), model = model)
#' @export
projection <- function(x = NULL, model) {
  states <- model$states
    
  pro <- rep_len(NA_real_, length(states))
  names(pro) <- states
  
  # NULL ... no projection
  if (is.null(x))
    return (pro)

  # a vector of state names. All others are set to 0
  if (is.character(x)) {
        xid <- pmatch(x, states)
        if (any(is.na(xid)))
          stop("Unknown state name(s): ", paste(x[is.na(xid)], collapse = ", "))
        pro[seq_along(states)[-xid]] <- 0
        return(pro)
  }
  
  # a unnamed vector of state IDs. All others are 0
  if(is.null(names(x)) && !any(is.na(match(x, seq_along(states))))) {
    pro[seq_along(states)[-x]] <- 0
    return(pro)
  }
  
  # a named vector with values for some dimensions
  # a vector with values for all dimensions (NAs are free dimensions)
  if (!is.null(names(x))) {
    xid <- pmatch(names(x), states)
    if (any(is.na(xid)))
      stop("Unknown state name(s): ", paste(names(x)[is.na(xid)], collapse = ", "))
    x <- x[states]
  }
  
  if (sum(x, na.rm = TRUE) > 1)
    stop("Sum of projection probabilities cannot be larger than 1!")
  
  names(x) <- states
  return(x)
}
