#' Extract the Policy from a POMDP/MDP
#'
#' Extracts the policy from a solved POMDP/MDP.
#'
#' A list (one entry per epoch) with the optimal policy.
#' For converged, infinite-horizon problems solutions, a list with only the 
#' converged solution is produced.
#' The policy is a data.frame consisting o:
#'
#' * Part 1: The value function with one column per state. 
#'   For POMDPs these are alpha vectors and for MDPs this is just
#'   one column with the state.
#'
#' * Part 2: One column with the optimal action.
#'
#' @family policy
#' 
#' @param x A solved [POMDP] object.
#' @return A list with the policy for each epoch.
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
#' sol <- solve_POMDP(model = Tiger, method = "incprune", horizon = 3, discount = 1)
#' sol
#'
#' policy(sol)
#' # Note: We see that it is initially better to listen till we make a decision in the final epoch. 
#' @export
policy <- function(x) {
  if(inherits(x, "MDP")) { 
    is_solved_MDP(x, stop = TRUE)
    return(x$solution$policy)
    
  } else
  is_solved_POMDP(x, stop = TRUE)
  n <- length(x$solution$pg)
  
  policy <- vector("list", n)
  for (i in seq_len(n)) {
    policy[[i]] <- cbind(x$solution$alpha[[i]],
      x$solution$pg[[i]][, "action", drop = FALSE])
  }
  return(policy)
}
  
.MDP_policy_from_POMDP <- function(x) {
  pg <- x$solution$pg
  
  ## all observation_probs should be the same!
  bs <- x$observation_prob[[1L]]
  
  # create a list ith epochs
  lapply(
    seq_along(pg),
    FUN = function(i) {
      rew <- reward_node_action(x, belief = bs, epoch = i)
      data.frame(
        state = colnames(bs),
        U = rew$reward,
        action = rew$action,
        pg_node = rew$pg_node
      )
    }
  )
}
