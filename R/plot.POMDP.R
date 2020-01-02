# plot policy graph


plot.POMDP <- function(x, y = NULL, ...) plot_policy_graph(x, ...)

plot_policy_graph <- function(x, belief = TRUE, legend = TRUE, cols = NULL, 
  engine = c("igraph", "visNetwork"), ...) {
  
  .solved_POMDP(x)
  
  engine <- match.arg(engine)
  switch(engine,
    igraph = .plot.igraph(x, belief, legend, cols, ...),
    visNetwork = .plot.visNetwork(x, belief, legend, cols, ...)
  )
}  


.plot.igraph <- function(x, belief, legend, cols, ...) {
  pg<- policy_graph(x, belief = belief, cols = cols)
  plot.igraph(pg, ...)
  
  if(legend && belief && !is.null(V(pg)$pie)) {
    legend("topright", legend = x$model$states, title = "Belief", 
      #horiz = TRUE,
      bty = "n",
      col = V(pg)$pie.color[[1]], 
      pch = 15
    )
  }
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

