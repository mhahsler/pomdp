# create a policy graph as an igraph object

policy_graph <- function(x, belief = TRUE, cols = NULL) {
 
  .solved_POMDP(x) 
  
  if(is.finite(x$solution$horizon)) stop("Only infinite horizon POMDPs have a plotable policy graph!")
  
  ## producing the optimal policy graph
  pg <- x$solution$pg
  
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
  init <- rep(":   ", nrow(x$solution$pg))
  init[x$solution$initial_pg_node] <- ": initial belief"
  
  V(policy_graph)$label <- paste0(x$solution$pg$node, init, 
    "\n", x$solution$pg$action) 
  
  # add belief proportions
  if(belief) {
    if(!is.null(x$solution$belief_states)) s <- list(
      belief = x$solution$belief_states[,1:length(x$model$states)], 
      optimal = reward(x, belief = x$solution$belief_states[,1:length(x$model$states)]))
    else s <- sample_belief_space(x)
    bp <- as.matrix(aggregate(s$belief, by = list(s$optimal$pg_node), mean, drop = FALSE)[, -1]) 
    
    pie_values <- lapply(1:nrow(bp), FUN = function(i) if(any(is.na(bp[i,]))) rep(1/ncol(bp), times = ncol(bp)) else bp[i,]) 
    
    ### Set1 from Colorbrewer
    number_of_states <- length(x$model$states)
    if(is.null(cols)) {
      cols <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
        "#A65628", "#F781BF", "#999999")
      if(number_of_states <= 9) cols <- cols[1:number_of_states]
      else cols <- rainbow(number_of_states)
    }else{
      if(length(cols) != number_of_states) stop("Number of colors is not the number of states.")
    }
    
    V(policy_graph)$shape <- "pie"
    V(policy_graph)$pie = pie_values
    V(policy_graph)$pie.color = list(cols)
  }    
  
  V(policy_graph)$size <- 40 
  E(policy_graph)$arrow.size  <- .5
  
  policy_graph
}


