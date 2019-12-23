# sample randomly from the belief space

sample_belief_space <- function(model, n = 1000, projection = NULL, epoch = 1) {
  alpha <- model$solution$alpha
  pg <- model$solution$pg
  if(is.list(alpha)) {
    alpha <- alpha[[epoch]]
    pg <- pg[[epoch]]
  }
  
  if(is.null(projection)) projection <- seq(ncol(alpha))
  if(is.character(projection)) projection <- pmatch(projection, model$model$states)
  
  d <- length(projection)
  
  # uniformly sample from a simplex.
  # https://cs.stackexchange.com/questions/3227/uniform-sampling-from-a-simplex)
  # Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986. 
  belief_states <- matrix(0, nrow = n, ncol = length(model$model$states))
  colnames(belief_states) <- model$model$states
  m <- cbind(0, matrix(runif(n*(d-1)), ncol = d-1), 1)
  belief_states[,projection] <- t(apply(m, MARGIN = 1, FUN = function(x) diff(sort(x))))
  
  vs <- .rew(belief_states, alpha)
  action <- pg$action[vs$pg_node]
  
  ret <- list(belief = belief_states, optimal = cbind(vs, action))
  ret
}