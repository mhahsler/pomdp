#' Extract the Policy Graph (as an igraph Object)
#' 
#' Convert the policy graph in a POMDP solution object into an igraph object.
#' 
#' 
#' @param x A POMDP object.
#' @param belief logical; add belief proportions as a pie chart in each node of
#' the graph? If belief points are provided by the solver, then these are used.
#' If a number is specified, then a random sample of that size is used instead
#' to calculate belief proportions.
#' @param col colors used for the states in the belief proportions.
#' @return An object of class igraph containing a directed graph. 
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @seealso \code{\link{solve_POMDP}}
#' @keywords graphs
#' @examples
#' 
#' data("Tiger")
#' sol <- solve_POMDP(model = Tiger)
#' sol
#' 
#' pg <- policy_graph(sol)
#' 
#' plot(pg)
#' 
#' @export
policy_graph <- function(x, belief = TRUE, col = NULL) {
 
  .solved_POMDP(x) 
  
  # create policy graph and belief proportions (average belief for each alpha vector)
  pg <- x$solution$pg[[1]]
  
  if(ncol(pg) < 3 || belief) {
    # FIXME: for pomdp-solve, we could seed with x$solution$belief_states
    bp <- .estimate_alpha_belief(x, 
      n = if(is.numeric(belief)) belief else 100)
    
    # missing belief points?
    missing_bp <- which(apply(is.na(bp), MARGIN = 1, any))
    if(length(missing_bp) > 0) stop("No belief points for policy graph node(s): ", 
      paste(missing_bp, collapse = ", "), ". Increase the number for parameter belief.")
    
    pg <- .calculate_pg(x, bp)
  }
  
  ## FIXME: try to make a graph from a not converged policy
  if(!x$solution$converged){ 
    if(nrow(x$solution$pg[[1]]) != nrow(x$solution$pg[[2]]))
      stop("Number of nodes the of last two epochs does not agree! Cannot create graph!")
    
    warning("POMDP has not converged. The last epoch in the policy tree may not form a graph! Use with caution!")
  }
  
  # producing a list containing arcs
  l <- list()
  list_of_arcs <- NULL
  #observations <- colnames(pg)[-c(1,2)]
  observations <- x$model$observations
  number_of_observations <- length(observations)
  l <- lapply(1:number_of_observations, FUN = function(i)
    data.frame(from = pg$node, to = pg[[observations[i]]], label = observations[i]))
  
  l <- do.call(rbind, l)
  l <- l[!is.na(l$to),] # remove links to nowhere ('-' in pg)
  
  # creating the initial graph
  policy_graph <- graph.edgelist(as.matrix(l[,1:2]))
  edge.attributes(policy_graph) <- list(label = l$label)
  edge.attributes(policy_graph) <- list(label = l$label)
  
  ### Note: the space helps with moving the id away from the pie cut.
  init <- rep(":   ", nrow(pg))
  init[x$solution$initial_pg_node] <- ": initial belief"
  
  V(policy_graph)$label <- paste0(pg$node, init, "\n", pg$action) 
  
  # add belief proportions
  if(belief) {
     
    pie_values <- lapply(1:nrow(bp), FUN = function(i) 
      if(any(is.na(bp[i,]))) structure(rep(1/ncol(bp), times = ncol(bp)), names = colnames(bp)) else bp[i,]) 
    
    ### Set1 from Colorbrewer
    number_of_states <- length(x$model$states)
    col <- .get_colors_descrete(number_of_states, col)
    
    V(policy_graph)$shape <- "pie"
    V(policy_graph)$pie = pie_values
    V(policy_graph)$pie.color = list(col)
  }    
  
  V(policy_graph)$size <- 40 
  E(policy_graph)$arrow.size  <- .5
  
  policy_graph
}


# use alpha_belief to create a policy graph 
# (for converged infinite horizon)
.calculate_pg <- function(x, alpha_belief) {
  
  #.solved_POMDP(x) 
  
  alpha <- x$solution$alpha[[1]]
  action <- x$solution$pg[[1]]$action
  
  ## FIXME: try to make a graph from a not converged policy
  if(!x$solution$converged || !is.infinite(x$solution$horizon)) 
    stop("Graphs can currently only be created for converged solutions for infinite-horizon problems.")
  
  pg <- t(sapply(1:nrow(alpha), FUN = function(i) {
    new_belief <- update_belief(x, 
      belief = alpha_belief[i,], action = action[i])
    
    structure(reward(x, belief = new_belief)$pg_node, 
      names = x$model$observations)
  }))
  
  cbind(data.frame(node = 1:nrow(alpha), action = action), pg)
} 

# estimate central beliefs for each alpha vector (infinite horizon)
# TODO: finite horizon
.estimate_alpha_belief <- function(x, n = 1000, simulate = FALSE, sim_horizon = n/10){
  
  alpha <- x$solution$alpha[[1]]
  
  if(!simulate)
    r <- reward(x, belief = sample_belief_space(x, n = n))
  else
    r <- reward(x, 
      simulate_POMDP(x, n = n/sim_horizon, 
        horizon = sim_horizon, visited_beliefs = TRUE), )
  
  belief <- t(sapply(1:nrow(alpha), FUN = function(i) 
    colMeans(r$belief[r$pg_node==i,, drop = FALSE])))
  belief
}
