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
policy <- function(x) x$solution$policy

.policy_MDP_from_POMDP <- function(x) {
  pg <- x$solution$pg
  bs <- x$observation_prob[['*']]
  
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
