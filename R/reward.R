# calculate the optimal reward from alpha vectors in a model

reward <- function(x, belief = NULL, epoch = 1) {
  .solved_POMDP(x)
  
  if(is.null(belief)) belief <- x$model$start
  
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
    action = pg$action[vs$pg_node]
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
