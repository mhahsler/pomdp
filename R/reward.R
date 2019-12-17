# calcualte the optimal reward from alpha vectors in a model

reward <- function(x, belief = "uniform", epoch = 1) {
  .solved_POMDP(x)
  
  start_belief <- .translate_belief(belief, x)
  
  ## alpha and pg is a list for finite horizon POMDPS
  if(is.list(x$solution$alpha)) {
    if(epoch > length(x$solution$alpha)) stop("The solution does not contain that many epochs. Either the horizon was set to less epochs or the solution converged earlier.")
    alpha <- x$solution$alpha[[epoch]]
    pg <- x$solution$pg[[epoch]]
  }
  else { 
    pg <- x$solution$pg
    alpha <- x$solution$alpha
  }
  
  rewards <- alpha %*% start_belief
  initial_pg_node <- which.max(rewards)
  total_expected_reward <- max(rewards)
  
  list(
    total_expected_reward = total_expected_reward, 
    belief = start_belief,
    pg_node = initial_pg_node,
    optimal_action = pg$action[initial_pg_node]
  )
}

## this helper is used by .belief_proportions in plot.POMDP.R
.rew <- function(belief, alpha) {
  if(!is.matrix(belief)) belief <- rbind(belief)
  r <- apply(belief, MARGIN = 1, FUN = function(b) {
    rewards <- alpha %*% b
    c(max(rewards), which.max(rewards))
  })
  r <- as.data.frame(t(r))
  colnames(r) <- c("reward", "segment")
  r$segment <- factor(r$segment, levels = 1:nrow(alpha))
  r
}
