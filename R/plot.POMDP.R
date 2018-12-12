plot.POMDP <- function(x, y = NULL, states=NULL , plot = TRUE, ...) {
  
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
  
  node_label <-  1:nrow(pg)
  
  # producing the pie values if we have belief proportions
  belief_proportions <- x$solution$belief_proportions
  if(!is.null(belief_proportions)) {
    number_of_states <- length(x$model$states)
    if (!is.null(states)) {
      belief_proportions <- belief_proportions[,states]
      for (i in nrow(belief_proportions)) {
        s <- sum(belief_proportions[i,])
        for (j in ncol(belief_proportions)) {
          belief_proportions[i,j] <- belief_proportions[i,j] / s
        }
      }
    }
    
    pie_values <- list()
    for (i in 1:nrow(pg)) {
      pie_values[[i]] <- as.numeric(belief_proportions[i,])
    }
  }
  
  edge_type <- as.integer(E(policy_graph)$label)
  E(policy_graph)$lty <- edge_type
  #E(policy_graph)$color <- "black"
  #E(policy_graph)$width <- edge_type
  #V(policy_graph)$color <-  "gray"
  #V(policy_graph)$frame.color[initial_node]  <-  "green"
  #V(policy_graph)$frame.color[1]  <-  "red"
  
  if(plot) {
    igraph_options(
      #vertex.size = 20, 
      #vertex.label.color="black", 
      #edge.color = "black", 
      #edge.label.cex = 2 , 
      edge.arrow.size =.3, 
      edge.curved = curve_multiple(policy_graph, start = .5)
      )
    
    if(!is.null(belief_proportions)) {
      plot.igraph(policy_graph, 
        #layout = cbind(seq(-1,1, length.out = length(V(policy_graph))),0) , 
        vertex.shape="pie", vertex.pie = pie_values,
        vertex.pie.color=list(rainbow(number_of_states)), 
        vertex.label=node_label, vertex.label.color = "white", ...)
      #, edge.label=NA , margin = c(0,0,0,0),
      # rescale = FALSE)
    }else{  
      plot.igraph(policy_graph, vertex.label=node_label, ...)
    }
    
    #legend('topleft' ,legend = x$model$observations , title="Observations" , lty = c(1:3))
    
    
    if(!is.null(belief_proportions)) {
      if(!is.null(states)) {
        legend("topright" , legend = x$model$states[states] , title = "Belief" , col = c(1:3), pch = 15)
      } else {
        legend("topright" , legend = x$model$states , title = "Belief" , col= rainbow(number_of_states) , pch = 15)
      }
    }
  }
  
  invisible(policy_graph)
}
