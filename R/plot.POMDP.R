policy_graph <- function(x, belief = TRUE, cols = NULL) {
 
  .solved_POMDP(x) 
  
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
  
  belief <- belief && !is.null(x$solution$belief_proportions)
  
  if(belief) {
    pie_values <- lapply(1:nrow(x$solution$belief_proportions), 
      FUN = function(i) {
        pv <- x$solution$belief_proportions[i,]
        if(any(is.na(pv))) pv <- rep(1, length(pv)) else pv
      })
    
    ### Set1 from Colorbrewer
    number_of_states <- ncol(x$solution$belief_proportions)
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
    legend("topright", legend = colnames(x$solution$belief_proportions), title = "Belief Proportions", 
      #horiz = TRUE,
      bty = "n",
      col = V(pg)$pie.color[[1]], 
      pch = 15
    )
  }
}
