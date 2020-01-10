# create a policy graph as an igraph object

policy_graph <- function(x, belief = TRUE, col = NULL) {
 
  .solved_POMDP(x) 
  
  if(!x$solution$converged) warning("POMDP has not converged. The last epoch in the policy tree may not form a graph! Use with caution!")
 
  ## try to make a graph from a not converged policy
  ## FIXME: This needs to be improved!
  if(!is.data.frame(x$solution$pg) && nrow(x$solution$pg[[1]]) != nrow(x$solution$pg[[2]]))
    stop("Number of nodes the of last two epoch does not aggree! Cannot create graph!")
  
  ## producing the optimal policy graph
  pg <- if(is.data.frame(x$solution$pg)) x$solution$pg else x$solution$pg[[1]]
  
  # producing a list containing arcs
  l <- list()
  list_of_arcs <- NULL
  observations <- colnames(pg)[-c(1,2)]
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
    if(!is.null(x$solution$belief_states) && !is.numeric(belief)) s <- list(
      belief = x$solution$belief_states[,1:length(x$model$states)], 
      optimal = reward(x, belief = x$solution$belief_states[,1:length(x$model$states)]))
    else s <- sample_belief_space(x, n = if(is.numeric(sample)) sample else 1000)
    
    bp <- as.matrix(aggregate(s$belief, by = list(s$optimal$pg_node), mean, drop = FALSE)[, -1]) 
    
    # missing belief points?
    missing_bp <- which(apply(is.na(bp), MARGIN = 1, any))
    if(length(missing_bp) > 0) warning("No belief points for policy graph node(s): ", 
      paste(missing_bp, collapse = ", "), ". Increase the number for parameter belief.")
     
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


