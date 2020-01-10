# sample randomly from the belief space

sample_belief_space <- function(model, projection = NULL, epoch = 1, n = 1000, random = TRUE) {
  alpha <- model$solution$alpha
  pg <- model$solution$pg
  if(is.list(alpha)) {
    alpha <- alpha[[epoch]]
    pg <- pg[[epoch]]
  }
  
  if(is.null(projection)) projection <- seq(ncol(alpha))
  if(is.character(projection)) projection <- pmatch(projection, model$model$states)
  
  d <- length(projection)
  if(d < 2) stop("Projection needs to be on 2 or more states.")
  if(!random && d > 3) stop("Non-random sampling is only available for 2D and 3D projections.")
  
  # empty belief states
  belief_states <- matrix(0, nrow = n, ncol = length(model$model$states))
  colnames(belief_states) <- model$model$states
  
  if(random) {
    # uniformly sample from a simplex.
    # https://cs.stackexchange.com/questions/3227/uniform-sampling-from-a-simplex)
    # Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986. 
    m <- cbind(0, matrix(runif(n*(d-1)), ncol = d-1), 1)
    belief_states[,projection] <- t(apply(m, MARGIN = 1, FUN = function(x) diff(sort(x))))
  } else if(d == 2) {
    b <- seq(0, 1, length.out = n)
    belief_states[, projection] <- cbind(b, 1 - b)
  } else if(d == 3) {
    ### Note: the number of points might not be exactly n!
    triangleCentres <- Ternary::TriangleCentres(round(n^.5), direction = 1)
    if(ncol(triangleCentres) != n) { 
      warning("Regular triangle grid produces ", ncol(triangleCentres), 
        " instead of ", n, " belief space samples!")
      belief_states <- matrix(0, nrow = ncol(triangleCentres), ncol = length(model$model$states))
      colnames(belief_states) <- model$model$states
    }
    
    belief_states[, projection] <- t(Ternary::XYToTernary(triangleCentres["x", ], triangleCentres["y", ],
      direction = 1))
    attr(belief_states, "TernaryTriangleCenters") <- triangleCentres 
}
  
  vs <- .rew(belief_states, alpha)
  action <- pg$action[vs$pg_node]
  
  ret <- list(belief = belief_states, optimal = cbind(vs, action))
  ret
}