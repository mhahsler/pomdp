#' POMDP Policy Graphs
#'
#' The function creates and plots the POMDP policy graph in a converged POMDP solution and the
#' policy tree for a finite-horizon solution.
#' uses `plot` in \pkg{igraph} with appropriate plotting options.
#'
#' Each policy graph node is represented by an alpha vector specifying a hyper plane segment. The convex hull of
#' the set of hyperplanes represents the the value function.
#' The policy specifies for each node an optimal action which is printed together with the node ID inside the node.
#' The arcs are labeled with observations.
#'
#' If available, a pie chart (or the color) in each node
#' represent an example of the belief that the agent has if it is in this node.
#' This can help with interpreting the policy graph.
#'
#' For finite-horizon solution a policy tree is produced.
#' The levels of the tree and the first number in the node label represent the epochs. Many algorithms produce
#' unused policy graph nodes which are filtered to produce a clean tree structure.
#' Non-converged policies depend on the initial belief and if an initial belief is
#' specified, then different nodes will be filtered and the tree will look different.
#'
#' First, the policy in the solved POMDP is converted into an [igraph] object using `policy_graph()`.
#' Example beliefs for the graph nodes are estimated using [estimate_belief_for_nodes()].
#' Finally, the igraph
#' object is visualized using the plotting function [igraph::plot.igraph()] or,
#' for interactive graphs, [visNetwork::visIgraph()].
#' @family policy
#'
#' @import igraph
#'
#' @param x object of class [POMDP] containing a solved and converged POMDP problem.
#' @param belief the initial belief is used to mark the initial belief state in the
#' grave of a converged solution and to identify the root node in a policy graph for a finite-horizon solution.
#' If `NULL` then the belief is taken from the model definition.
#' @param show_belief logical; show estimated belief proportions as a pie chart in each node?
#' @param legend logical; display a legend for colors used belief proportions?
#' @param engine The plotting engine to be used. For `"visNetwork"`, `flip.y = FALSE` can be used
#'   to show the root node on top.
#' @param col colors used for the states.
#' @param ... parameters are passed on to `policy_graph()`, [estimate_belief_for_nodes()] and the functions
#'   they use. Also, plotting options are passed on to the plotting engine [igraph::plot.igraph()]
#'   or [visNetwork::visIgraph()].
#'
#' @returns
#' - `policy_graph()` returns the policy graph as an igraph object.
#' - `plot_policy_graph()` returns invisibly what the plotting engine returns.
#'
#' @keywords hplot graphs
#' @examples
#' data("Tiger")
#'
#' ## policy graphs for converged solutions
#' sol <- solve_POMDP(model = Tiger)
#' sol
#'
#' policy_graph(sol)
#'
#' ## visualization
#' plot_policy_graph(sol)
#'
#' ## use a different graph layout (circle and manual; needs igraph)
#' library("igraph")
#' plot_policy_graph(sol, layout = layout.circle)
#' plot_policy_graph(sol, layout = rbind(c(1,1), c(1,-1), c(0,0), c(-1,-1), c(-1,1)))
#'
#' ## hide labels and legend
#' plot_policy_graph(sol, edge.label = NA, vertex.label = NA, legend = FALSE)
#'
#' ## add a plot title
#' plot_policy_graph(sol, main = sol$name)
#'
#' ## custom larger vertex labels (A, B, ...)
#' plot_policy_graph(sol,
#'   vertex.label = LETTERS[1:nrow(policy(sol)[[1]])],
#'   vertex.label.cex = 2,
#'   vertex.label.color = "white")
#'
#' ## plotting the igraph object directly
#' ## (e.g., using the graph in the layout and to change the edge curvature)
#' pg <- policy_graph(sol)
#' plot(pg,
#'   layout = layout_as_tree(pg, root = 3, mode = "out"),
#'   edge.curved = curve_multiple(pg, .2))
#'
#' ## changes labels
#' plot(pg,
#'   edge.label = abbreviate(E(pg)$label),
#'   vertex.label = vertex_attr(pg)$label,
#'   vertex.size = 20)
#'
#' ## plot interactive graphs using the visNetwork library.
#' ## Note: the pie chart representation is not available, but colors are used instead.
#' plot_policy_graph(sol, engine = "visNetwork")
#'
#' ## add smooth edges and a layout (note, engine can be abbreviated)
#' plot_policy_graph(sol, engine = "visNetwork", layout = "layout_in_circle", smooth = TRUE)
#'
#' ## policy trees for finite-horizon solutions
#' sol <- solve_POMDP(model = Tiger, horizon = 4, method = "incprune")
#'
#' policy_graph(sol)
#'
#' plot_policy_graph(sol)
#' # Note: the first number in the node id is the epoch.
#'
#' # plot the policy tree for an initial belief of 90% that the tiger is to the left
#' plot_policy_graph(sol, belief = c(0.9, 0.1))
#'
#' # Plotting a larger graph (see ? igraph.plotting for plotting options)
#' sol <- solve_POMDP(model = Tiger, horizon = 10, method = "incprune")
#'
#' plot_policy_graph(sol, vertex.size = 8, edge.arrow.size = .1,
#'   vertex.label.cex = .5, edge.label.cex = .5)
#' @export
plot_policy_graph <- function(x,
  belief = NULL,
  show_belief = TRUE,
  legend = TRUE,
  engine = c("igraph", "visNetwork"),
  col = NULL,
  ...) {
  engine <- match.arg(engine)
  switch(
    engine,
    igraph = .plot.igraph(
      x,
      belief,
      show_belief = show_belief,
      legend = legend,
      col = col,
      ...
    ),
    visNetwork = .plot.visNetwork(
      x,
      belief,
      show_belief = show_belief,
      legend = legend,
      col = col,
      ...
    )
  )
}


.plot.igraph <-
  function(x,
    belief = NULL,
    show_belief,
    legend,
    col,
    edge.curved = NULL,
    ...) {
    pg <-
      policy_graph(x, belief, show_belief = show_belief, col = col, ...)
    
    if (is.null(edge.curved))
      edge.curved <- .curve_multiple_directed(pg)
    
    plot.igraph(pg, edge.curved = edge.curved, ...)
    
    if (legend && show_belief && !is.null(vertex_attr(pg)$pie)) {
      legend(
        "topright",
        legend = x$states,
        title = "Belief",
        #horiz = TRUE,
        bty = "n",
        col = vertex_attr(pg)$pie.color[[1]],
        pch = 15
      )
    }
  }

### fix the broken curve_multiple for directed graphs (igraph_1.2.2)
.curve_multiple_directed <- function(graph, start = 0.3) {
  el <-  as_edgelist(graph, names = FALSE)
  o <- apply(el, 1, order)[1, ]
  el <-
    apply(
      el,
      1,
      FUN = function(x)
        paste(sort(x), collapse = ":")
    )
  cu <- stats::ave(
    rep(NA, length(el)),
    el,
    FUN = function(x) {
      if (length(x) == 1) {
        return(0)
      }
      else {
        return(seq(-start, start, length = length(x)))
      }
    }
  )
  
  cu[o == 2] <- cu[o == 2] * -1
  cu
}

# plot policy graph using visNetwork

# Note: legend is not used right now!
.plot.visNetwork <-
  function(x,
    belief = NULL,
    show_belief = TRUE,
    legend = NULL,
    col = NULL,
    smooth = list(type = "continuous"),
    layout = NULL,
    ...) {
    check_installed("visNetwork")
    
    unconverged <-
      !x$solution$converged || length(x$solution$pg) > 1
    if (is.null(layout))
      layout <-
      ifelse(unconverged, "layout_as_tree", "layout_nicely")
    
    pg <-
      policy_graph(x, belief, show_belief = show_belief, col = col)
    
    ### add tooltip
    #V(pg)$title <- paste(htmltools::tags$b(V(pg)$label)
    vertex_attr(pg, "title") <- paste(vertex_attr(pg, "label"),
      lapply(
        vertex_attr(pg, "pie"),
        FUN = function(b) {
          knitr::kable(cbind(belief = b), digits = 3, format = "html")
        }
      ))
    
    ### colors
    if (show_belief) {
      # winner
      #V(pg)$color <- V(pg)$pie.color[[1]][sapply(V(pg)$pie, which.max)]
      
      # mixing in rgb space
      vertex_attr(pg, "color") <- sapply(
        seq(length(V(pg))),
        FUN = function(i) {
          mix <-
            t(grDevices::col2rgb(vertex_attr(pg, "pie.color")[[1]]) %*% vertex_attr(pg, "pie")[[i]])
          if(any(is.na(mix))) mix <- cbind(255, 255, 255)
          grDevices::rgb(mix, maxColorValue = 255)
        }
      )
      
      # mixing in hsv space
      #V(pg)$color <- sapply(seq(length(V(pg))), FUN = function(i)
      #  do.call(hsv, as.list(rgb2hsv(col2rgb(V(pg)$pie.color[[1]])) %*% V(pg)$pie[[i]])))
    }
    
    visNetwork::visIgraph(pg,
      idToLabel = FALSE,
      layout = layout,
      smooth = smooth,
      ...) %>%
      visNetwork::visOptions(
        highlightNearest = list(enabled = TRUE, degree = 0),
        nodesIdSelection = TRUE
      )
  }
