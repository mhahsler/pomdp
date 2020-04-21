#' Visualize a POMDP Policy Graph
#' 
#' The function plots the POMDP policy graph in an object of class POMDP. It
#' uses \code{plot} in \pkg{igraph} with appropriate plotting options.
#' 
#' The method \code{plot} for objects of class POMDP calls for convenience
#' \code{plot_policy_graph}.
#' 
#' The policy graph nodes represent segments in the value function. Each
#' segment represents one or more believe states. The pie chart in each node
#' (if available) represent the average belief proportions of the belief states
#' belonging to the node/segment.
#' 
#' The built in plotting engines are \pkg{igraph} and \pkg{visNetwork}. The
#' additional arguments specified in \code{...} are passed on to the engine
#' plotting function.  For \pkg{igraph} this is
#' \code{\link[igraph]{plot.igraph}} (see \code{\link[igraph]{plot.common}} for
#' available options). For \pkg{visNetwork} this is
#' \code{\link[visNetwork]{visIgraph}}.
#' 
#' Other plotting libraries can be used by creating a policy graph (as an
#' igraph object) using \code{\link{policy_graph}} and converting it into a
#' suitable representation for that library.
#' 
#' @aliases plot.POMDP plot_policy_graph plot
#' @param x object of class POMDP containing a solved POMDP problem.
#' @param y ignored.
#' @param belief logical; display belief proportions as a pie chart in each
#' node.
#' @param legend logical; display a legend for colors used belief proportions?
#' @param engine The plotting engine to be used.
#' @param col colors used for the states.
#' @param \dots plotting options passed on to the plotting engine (see Details
#' section).
#' @seealso \code{\link{solve_POMDP}}, \code{\link{policy_graph}}.
#' 
#' From: \pkg{igraph} \code{\link[igraph]{plot.igraph}},
#' \code{\link[igraph]{igraph_options}}, \code{\link[igraph]{plot.common}}.
#' 
#' From \pkg{visNetwork}: \code{\link[visNetwork]{visIgraph}}.
#' @keywords hplot
#' @examples
#' 
#' data("Tiger")
#' sol <- solve_POMDP(model = Tiger)
#' sol
#' 
#' ## policy graph
#' policy_graph(sol)
#' 
#' ## visualization
#' plot(sol)
#' 
#' ## use a different graph layout (circle and manual; needs igraph)
#' library("igraph")
#' plot(sol, layout = layout.circle)
#' plot(sol, layout = rbind(c(1,1), c(1,-1), c(0,0), c(-1,-1), c(-1,1)))
#' 
#' ## hide labels and legend
#' plot(sol, edge.label = NA, vertex.label = NA, legend = FALSE)
#' 
#' ## add a plot title
#' plot(sol, main = sol$model$name)
#' 
#' ## custom larger vertex labels (A, B, ...)
#' plot(sol, 
#'   vertex.label = LETTERS[1:nrow(policy(sol)[[1]])], 
#'   vertex.label.cex = 2,
#'   vertex.label.color = "white")
#' 
#' ## plotting using the graph object 
#' ## (e.g., using the graph in the layout and to change the edge curvature)
#' pg <- policy_graph(sol)
#' plot(pg, 
#'   layout = layout_as_tree(pg, root = 3, mode = "out"),
#'   edge.curved = curve_multiple(pg, .2))
#' 
#' ## changes labels
#' plot(pg, 
#'   edge.label = abbreviate(E(pg)$label),
#'   vertex.label = sol$solution$pg$action,
#'   vertex.size = 10)
#' 
#' ## plot interactive graphs using the visNetwork library
#' plot(sol, engine = "visNetwork")
#' 
#' ## add smooth edges and a layout (note, engine can be abbreviated)
#' plot(sol, engine = "vis", layout = "layout_in_circle", smooth = TRUE)
#' @export
plot.POMDP <- function(x, y = NULL, ...) plot_policy_graph(x, ...)

#' @rdname plot.POMDP
#' @export
plot_policy_graph <- function(x, belief = TRUE, legend = TRUE, 
  engine = c("igraph", "visNetwork"), col = NULL, ...) {
  
  .solved_POMDP(x)
  
  engine <- match.arg(engine)
  switch(engine,
    igraph = .plot.igraph(x, belief, legend, col, ...),
    visNetwork = .plot.visNetwork(x, belief, legend, col, ...)
  )
}  


.plot.igraph <- function(x, belief, legend, col, edge.curved = NULL, ...) {
  pg <- policy_graph(x, belief = belief, col = col)
  
  if(is.null(edge.curved)) edge.curved <- .curve_multiple_directed(pg)
  
  plot.igraph(pg, edge.curved = edge.curved, ...)
  
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
.curve_multiple_directed <- function(graph, start = 0.3) {
   el <-  as_edgelist(graph, names = FALSE)
   o <- apply(el, 1, order)[1,]
   el <- apply(el, 1, FUN = function(x) paste(sort(x), collapse = ":"))
   cu <- ave(rep(NA, length(el)), el, FUN = function(x) {
     if (length(x) == 1) {
       return(0)
     }
     else {
       return(seq(-start, start, length = length(x)))
     }
   }
   )
   
   cu[o==2] <- cu[o==2] * -1
   cu
 }

