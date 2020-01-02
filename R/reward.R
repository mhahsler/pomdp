# calcualte the optimal reward from alpha vectors in a model

reward <- function(x, belief = "uniform", epoch = 1) {
  .solved_POMDP(x)
  
  belief <- .translate_belief(belief, x)
  
  ## alpha and pg is a list for finite horizon POMDPS
  if(is.list(x$solution$alpha)) {
    if(epoch > length(x$solution$alpha)) stop("The solution does not contain that many epochs. Either the horizon was set to less epochs or the solution converged earlier.")
    alpha <- x$solution$alpha[[epoch]]
    pg <- x$solution$pg[[epoch]]
  } else { 
    pg <- x$solution$pg
    alpha <- x$solution$alpha
  }
  
  vs <- .rew(belief, alpha)
  
  list(
    belief = belief,
    total_expected_reward = vs$reward, 
   pg_node = vs$pg_node,
    optimal_action = pg$action[vs$pg_node]
  )
}

## this reward helper
.rew <- function(belief, alpha) {
  if(!is.matrix(belief)) belief <- rbind(belief)
  r <- apply(belief, MARGIN = 1, FUN = function(b) {
    rewards <- alpha %*% b
    c(max(rewards), which.max(rewards))
  })
  r <- as.data.frame(t(r))
  colnames(r) <- c("reward", "pg_node")
  r$pg_node <- factor(r$pg_node, levels = 1:nrow(alpha))
  r
}
