#' Calculate the Reward for a POMDP Solution
#'
#' This function calculates the expected total reward for a POMDP solution
#' given a starting belief state. The value is calculated using the value function stored 
#' in the POMDP solution. In addition, the policy graph node that represents the belief state
#' and the optimal action can also be returned using `reward_node_action()`.
#'
#' @family policy
#' 
#' @param x a solved [POMDP] object.
#' @param belief specification of the current belief state (see argument start
#' in [POMDP] for details). By default the belief state defined in
#' the model as start is used. Multiple belief states can be specified as rows in a matrix.
#' @param epoch return reward for this epoch. Use 1 for converged policies.
#' 
#' @returns `reward()` returns a vector of reward values, one for each belief if a matrix is specified.
#' 
#' `reward_node_action()` returns a list with the components 
#' \item{belief_state}{the belief state specified in `belief`.} 
#' \item{reward}{the total expected reward given a belief and epoch. } 
#' \item{pg_node}{the policy node that represents the belief state.} 
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
#'
#' # return reward, the initial node in the policy graph and the optimal action for
#' # two beliefs. 
#' reward_node_action(sol, belief = rbind(c(.5, .5), c(.9, .1)))
#'
#' # manually combining reward with belief space sampling to show the value function
#' # (color signifies the optimal action)
#' samp <- sample_belief_space(sol, n = 200)
#' rew <- reward_node_action(sol, belief = samp)
#' plot(rew$belief[,"tiger-right"], rew$reward, col = rew$action, ylim = c(0, 15))
#' legend(x = "top", legend = levels(rew$action), title = "action", col = 1:3, pch = 1)
#' 
#' # this is the piecewise linear value function from the solution
#' plot_value_function(sol, ylim = c(0, 10))
#' @export
reward <- function(x, belief = NULL, epoch = 1) {
  reward_node_action(x, belief, epoch)$reward
}

#' @rdname reward
#' @export
reward_node_action <- function(x, belief = NULL, epoch = 1) {
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
