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
  
  # deleting the reset arc
  #list_of_arcs <- list_of_arcs[!rowSums(list_of_arcs[,1:2] == 1) == 2,]
  
  # creating the initial graph
  policy_graph <- graph.edgelist(as.matrix(l[,1:2]))
  edge.attributes(policy_graph) <- list(label = l$label)
  edge.attributes(policy_graph) <- list(label = l$label)
  
  ### Note: the space helps with moving the id away from the pie cut.
  init <- rep(":   ", nrow(x$solution$pg))
  init[x$solution$initial_pg_node] <- ": start"
  
  V(policy_graph)$label <- paste0(x$solution$pg$node, init, 
    "\n", x$solution$pg$action) 
  
  if(belief) {
    bp <- .belief_proportions(x$solution$alpha, 
      belief_states = x$solution$belief_states[,1:ncol(x$solution$alpha)])
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


.belief_proportions <- function(alpha, belief_states = NULL, n = 10000) {
  
  ### sample n belief states uniformly of nore are available
  if(is.null(belief_states)) {
    d <- ncol(alpha)
    # this is not uniform
    #grid <- matrix(runif(n*d), ncol = d)
    #belief_states <- sweep(grid, MARGIN = 1, STATS = rowSums(grid), "/")

    # uniformly sample from a simplex.
    # https://cs.stackexchange.com/questions/3227/uniform-sampling-from-a-simplex)
    # Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986. 
    m <- cbind(0, matrix(runif(n*(d-1)), ncol = d-1), 1)
    belief_states <- t(apply(m, MARGIN = 1, FUN = function(x) diff(sort(x))))
    
  } else belief_states <- belief_states[,1:ncol(alpha)] ### in case there is a node column
  vs <- .rew(belief_states, alpha)
  vs <- aggregate(belief_states, by = list(vs$segment), mean, drop = FALSE)[,-1]
  colnames(vs) <- colnames(alpha)
  as.matrix(vs)
}


### fix the broken curve_multiple for directed graphs (igraph_1.2.2)
# curve_multiple_fixed <- function(graph, start = 0.5) 
# {
#   el <-  as_edgelist(graph, names = FALSE)
#   o <- apply(el, 1, order)[1,]
#   el <- apply(el, 1, FUN = function(x) paste(sort(x), collapse = ":"))
#   cu <- ave(rep(NA, length(el)), el, FUN = function(x) {
#     if (length(x) == 1) {
#       return(0)
#     }
#     else {
#       return(seq(-start, start, length = length(x)))
#     }
#   }
#   )
#   
#   cu[o==2] <- cu[o==2] * -1
#   cu
# }




plot.POMDP <- function(x, y = NULL, belief = TRUE, legend = TRUE, cols = NULL, ...) {
  
  pg<- policy_graph(x, belief = belief, cols = cols)
  plot.igraph(pg, ...)
  
  if(legend && belief && !is.null(V(pg)$pie)) {
    legend("topright", legend = x$model$states, title = "Belief Proportions", 
      #horiz = TRUE,
      bty = "n",
      col = V(pg)$pie.color[[1]], 
      pch = 15
    )
  }
}
