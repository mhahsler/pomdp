#' Extract the Policy from a POMDP/MDP
#'
#' Extracts the policy from a solved POMDP/MDP.
#'
#' A list (one entry per epoch) with the optimal policy.
#' For converged, infinite-horizon problems solutions, a list with only the 
#' converged solution is produced.
#' The policy is a data.frame consisting o:
#'
#' * Part 1: The value function with one column per state. (For MDPs this is just
#' one column with the state).
#'
#' * Part 2: One column with the optimal action.
#'
#' @param x A solved [POMDP] object.
#' @return A list with the policy for each epoch.
#' @author Michael Hahsler
#' @keywords graphs
#' @examples
#' data("Tiger")
#' sol <- solve_POMDP(model = Tiger)
#' sol
#'
#' # policy with value function, optimal action and transitions for observations.
#' policy(sol)
#' @export
policy <- function(x) x$solution$policy

.policy_MDP_from_POMDP <- function(x) {
  pg <- x$solution$pg
  bs <- x$model$observation_prob[['*']]
  
  # create a list ith epochs
  lapply(
    seq_along(pg),
    FUN = function(i) {
      rew <- reward(x, belief = bs, epoch = i)
      data.frame(
        state = colnames(bs),
        U = rew$reward,
        action = rew$action,
        pg_node = rew$pg_node
      )
    }
  )
}
