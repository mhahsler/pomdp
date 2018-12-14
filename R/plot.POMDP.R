policy_graph <- function(x) {
  ## producing the optimal policy graph
  pg <- x$solution$pg
  
  # adjacency matrix
  adjm <-  matrix(data=0, nrow=nrow(pg) , ncol=nrow(pg))
  for (i in 1 : nrow(pg)) {
    for (j in 3 : ncol(pg)) {
      adjm[i,pg[i,j]] <- 1
    }
  }
  
  # producing a list containing arcs
  l <- list()
  list_of_arcs <- NULL
  number_of_observations <- length(x$model$observations)
  observations <- x$model$observations
  for (i in 1:number_of_observations) {
    l[[i]] <- data.frame(from = pg$belief, to = pg[[observations[i]]], label = observations[i])
    list_of_arcs <- rbind(list_of_arcs,l[[i]])
  }
  # deleting the reset arc
  list_of_arcs <- list_of_arcs[!rowSums(list_of_arcs[,1:2] == 1) == 2,]
  
  # creating the initial graph
  policy_graph <- graph.edgelist(as.matrix(list_of_arcs[,1:2]))
  edge.attributes(policy_graph) <- list(label = list_of_arcs$label)
  edge_type <- as.integer(E(policy_graph)$label)
  #E(policy_graph)$lty <- edge_type

  #labels(V(policy_graph)) <- paste0(x$solution$pg$belief, ": ", x$solution$pg$action)
  ### Note: the space helps with moving the id away from the pie cut.
 
  init <- rep(":   ", nrow(x$solution$pg))
  init[x$solution$initial_belief_state] <- ": initial"
   
  V(policy_graph)$label <- paste0(x$solution$pg$belief, init, 
    "\n", x$solution$pg$action) 
  
  policy_graph
}

plot.POMDP <- function(x, y = NULL, vertex.size = 40, edge.arrow.size =.5, 
  vertex.frame.color = "grey", ...) {
  
  policy_graph <- policy_graph(x)

  if(!is.null(x$solution$belief_proportions)) {
    # producing the pie values if we have belief proportions
    belief_proportions <- x$solution$belief_proportions
    if(!is.null(belief_proportions)) {
      number_of_states <- length(x$model$states)
      #  if (!is.null(states)) {
      #    belief_proportions <- belief_proportions[,states]
      #    for (i in nrow(belief_proportions)) {
      #      s <- sum(belief_proportions[i,])
      #      for (j in ncol(belief_proportions)) {
      #        belief_proportions[i,j] <- belief_proportions[i,j] / s
      #      }
      #    }
      #  }
      
      pie_values <- list()
      for (i in 1:nrow(x$solution$pg)) {
        pie_values[[i]] <- as.numeric(belief_proportions[i,])
      }
    }
   
    ### Set1 from Colorbrewer
    cols <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
      "#A65628", "#F781BF", "#999999")
    if(number_of_states < 10) cols <- cols[1:number_of_states]
    else cols <- rainbow(number_of_states)
    
    plot.igraph(policy_graph, 
      #layout = cbind(seq(-1,1, length.out = length(V(policy_graph))),0) , 
      vertex.shape = "pie", 
      vertex.pie = pie_values,
      vertex.pie.color = list(cols),
      #vertex.label = 1:nrow(x$solution$pg), 
      #vertex.label.color = "white",
      edge.curved = curve_multiple_fixed(policy_graph),
      vertex.size = vertex.size, edge.arrow.size = edge.arrow.size,
      vertex.frame.color = vertex.frame.color,
      ...)
    #, edge.label=NA , margin = c(0,0,0,0),
    # rescale = FALSE)
  }else{  
    plot.igraph(policy_graph, 
      edge.curved = curve_multiple_fixed(policy_graph),
      vertex.size = vertex.size, edge.arrow.size = edge.arrow.size,
      vertex.frame.color = vertex.frame.color,
      ...)
  }
  
  #legend('topleft' ,legend = x$model$observations , title="Observations" , lty = c(1:3))
  
  
  if(!is.null(x$solution$belief_proportions)) {
#    if(!is.null(states)) {
#      legend("topright" , legend = x$model$states[states] , title = "Belief" , col = c(1:3), pch = 15)
 #   } else {
      legend("topright", legend = x$model$states, title = "Belief Proportions", 
        #horiz = TRUE,
        #bty = "n",
        col= cols, pch = 15
        )
  #  }
  }
}