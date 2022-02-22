#' Calculate the Reward for a POMDP Solution
#'
#' This function calculates the expected total reward for a POMDP solution
#' given a starting belief state.
#'
#' The value is calculated using the value function stored in the POMDP
#' solution.
#'
#' @family policy
#' 
#' @param x a solved [POMDP] object.
#' @param belief specification of the current belief state (see argument start
#' in [POMDP] for details). By default the belief state defined in
#' the model as start is used.
#' @param epoch return reward for this epoch. Default is the first epoch.
#' 
#' @return A list with the components 
#' \item{reward}{the total expected reward
#' given a belief and epoch. } 
#' \item{belief_state}{the belief state specified
#' in `belief`.} 
#' \item{pg_node}{the policy node that represents the belief
#' state.} 
#' \item{action}{the optimal action.}
#' @author Michael Hahsler
#' @examples
#' data("Tiger")
#' sol <- solve_POMDP(model = Tiger)
#'
#' # if no start is specified, a uniform belief is used.
#' reward(sol)
#'
#' # we have additional information that makes us believe that the tiger
#' # is more likely to the left.
#' reward(sol, belief = c(0.85, 0.15))
#'
#' # we start with strong evidence that the tiger is to the left.
#' reward(sol, belief = "tiger-left")
#'
#' # Note that in this case, the total discounted expected reward is greater
#' # than 10 since the tiger problem resets and another game staring with
#' # a uniform belief is played which produces additional reward.
#' @export
reward <- function(x, belief = NULL, epoch = 1) {
  .solved_POMDP(x)
  
  if (is.null(belief))
    belief <- x$start
  
  belief <- .translate_belief(belief, x)
  
  ## translate for converged POMDPs
  e <- .get_pg_index(x, epoch)
  
  alpha <- x$solution$alpha[[e]]
  pg <- x$solution$pg[[e]]
  
  vs <- .rew(belief, alpha)
  
  list(
    belief = belief,
    reward = vs$reward,
    pg_node = vs$pg_node,
    action = factor(pg$action[vs$pg_node], levels = x$actions)
  )
}

## this reward helper
.rew <- function(belief, alpha) {
  if (!is.matrix(belief))
    belief <- rbind(belief)
  r <- apply(
    belief,
    MARGIN = 1,
    FUN = function(b) {
      rewards <- alpha %*% b
      c(max(rewards), which.max(rewards))
    }
  )
  r <- as.data.frame(t(r))
  colnames(r) <- c("reward", "pg_node")
  #r$pg_node <- factor(r$pg_node, levels = 1:nrow(alpha))
  r
}
