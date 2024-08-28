#' Extract the Policy from a POMDP
#'
#' Extracts the policy from a solved POMDP.
#'
#' A list (one entry per epoch) with the optimal policy.
#' For converged, infinite-horizon problems solutions, a list with only the
#' converged solution is produced.
#' For a POMDP, the policy is a data.frame consisting of:
#'
#' * Part 1: The alpha vectors for the belief states (defines also the 
#'            utility of the belief). The columns have 
#'            the names of states.
#' * Part 2: The last column named `action` contains the prescribed action.
#'
#' For an MDP, the policy is a data.frame with columns for:
#'
#' * `state`: The state.
#' * `U`: The state's value (discounted expected utility U) if the policy 
#'    is followed
#' * `action`: The prescribed action.
#'
#' @family policy
#'
#' @param x A solved [POMDP] object.
#' @param drop logical; drop the list for converged, epoch-independent policies.
#' @return A list with the policy for each epoch. Converged policies
#'  have only one element. If `drop = TRUE` then the policy is returned 
#'  without a list.
#' @author Michael Hahsler
#' @keywords graphs
#' @examples
#' data("Tiger")
#'
#' # Infinite horizon
#' sol <- solve_POMDP(model = Tiger)
#' sol
#'
#' # policy with value function, optimal action and transitions for observations.
#' policy(sol)
#' plot_value_function(sol)
#'
#' # Finite horizon (we use incremental pruning because grid does not converge)
#' sol <- solve_POMDP(model = Tiger, method = "incprune", 
#'   horizon = 3, discount = 1)
#' sol
#'
#' policy(sol)
#' # Note: We see that it is initially better to listen till we make 
#' #       a decision in the final epoch.
#' @importFrom markovDP policy
#' @export
policy.POMDP <- function(x, drop = TRUE) {
  is_solved_POMDP(x, stop = TRUE)
  n <- length(x$solution$pg)
  
  policy <- vector("list", n)
  for (i in seq_len(n)) {
      policy[[i]] <- cbind(x$solution$alpha[[i]],
                           x$solution$pg[[i]][, "action", drop = FALSE])
  }
  
  if(drop && length(policy) == 1)
    policy <- policy[[1]]
  
  policy
}