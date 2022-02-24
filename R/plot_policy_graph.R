#' Visualize a POMDP Policy Graph
#'
#' The function plots the POMDP policy graph in a converged POMDP solution. It
#' uses `plot` in \pkg{igraph} with appropriate plotting options.
#'
#' The function only plots **converged policy graphs.**
#' The policy graph nodes represent the segments in the value function. Each
#' segment represents one or more believe states. If available, a pie chart (or the color) in each node
#' represent the average belief of the belief states
#' belonging to the node/segment. This can help with interpreting the policy graph.
#'
#' First, the policy in the solved POMDP is converted into an [igraph] object using `policy_graph()`.
#' Average beliefs for the graph nodes are estimated using `estimate_belief_for_node()` and then the igraph
#' object is visualized using the plotting function [igraph::plot.igraph()] or,
#' for interactive graphs, [visNetwork::visIgraph()].
#'
#' `estimate_belief_for_nodes()` estimated the central belief for each node/segment of the value function
#' by generating/sampling a large set of possible belief points, assigning them to the segments and then averaging
#' the belief over the points assigned to each segment. If no belief point is generated for a segment, then a
#' warning is produced. In this case, the number of sampled points can be increased.
#'
#' @family policy
#'
#' @import igraph
#'
#' @param x object of class [POMDP] containing a solved and converged POMDP problem.
#' @param belief logical; estimate belief proportions? If `TRUE` then `estimate_belief_for_nodes()` is used
#'  and the belief is visualized as a pie chart in each node.
#' @param legend logical; display a legend for colors used belief proportions?
#' @param engine The plotting engine to be used.
#' @param col colors used for the states.
#' @param ... parameters are passed on to `policy_graph()`, `estimate_belief_for_nodes()` and the functions
#'   they use. Also, plotting options are passed on to the plotting engine [igraph::plot.igraph()]
#'   or [visNetwork::visIgraph()].
#'
#' @returns
#' - `plot_policy_graph()` returns invisibly what the plotting engine returns.
#' - `policy_graph()` returns the policy graph as an igraph object.
#' - `estimate_belief_for_nodes()` returns a matrix with the central belief for each node.
#'
#' @keywords hplot graphs
#' @examples
#' data("Tiger")
#' sol <- solve_POMDP(model = Tiger)
#' sol
#'
#' ## policy graph
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
#'   vertex.label = V(pg)$label,
#'   vertex.size = 20)
#'
#' ## plot interactive graphs using the visNetwork library.
#' ## Note: the pie chart representation is not available, but colors are used instead.
#' plot_policy_graph(sol, engine = "visNetwork")
#'
#' ## add smooth edges and a layout (note, engine can be abbreviated)
#' plot_policy_graph(sol, engine = "vis", layout = "layout_in_circle", smooth = TRUE)
#'
#' ## estimate the central belief for the graph nodes. We use here method "simulate".
#' ## For infinite horizon problems, the simulation horizon  has to be specified.
#' ## See simulate_POMDP().
#' estimate_belief_for_nodes(sol, method = "simulate", horizon = 10)
#' @export
plot_policy_graph <- function(x,
  belief = TRUE,
  legend = TRUE,
  engine = c("igraph", "visNetwork"),
  col = NULL,
  ...) {
  .solved_POMDP(x)
  
  engine <- match.arg(engine)
  switch(
    engine,
    igraph = .plot.igraph(x, belief, legend, col, ...),
    visNetwork = .plot.visNetwork(x, belief, legend, col, ...)
  )
}


.plot.igraph <-
  function(x, belief, legend, col, edge.curved = NULL, ...) {
    pg <- policy_graph(x, belief = belief, col = col, ...)
    
    if (is.null(edge.curved))
      edge.curved <- .curve_multiple_directed(pg)
    
    plot.igraph(pg, edge.curved = edge.curved, ...)
    
    if (legend && belief && !is.null(V(pg)$pie)) {
      legend(
        "topright",
        legend = x$states,
        title = "Belief",
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



#' @rdname plot_policy_graph
#' @export
policy_graph <- function(x, belief = TRUE, col = NULL, ...) {
  .solved_POMDP(x)
  
  ## FIXME: try to make a graph from a not converged policy
  if (!x$solution$converged || length(x$solution$pg) > 1)
    stop(
      "Solution has not converged. Creating an igraph object is only supported for converged policies!"
    )
  
  # create policy graph and belief proportions (average belief for each alpha vector)
  pg <- x$solution$pg[[1]]
  
  if (belief) {
    # FIXME: for pomdp-solve, we could seed with x$solution$belief_states
    bp <- estimate_belief_for_nodes(x, ...)
    
    # missing belief points?
    missing_bp <- which(apply(is.na(bp), MARGIN = 1, any))
    if (length(missing_bp) > 0)
      warning(
        "No belief points sampled for policy graph node(s): ",
        paste(missing_bp, collapse = ", "),
        ". Increase the number for parameter belief (number of sampled points)."
      )
  }
  
  # producing a list containing arcs
  l <- list()
  list_of_arcs <- NULL
  #observations <- colnames(pg)[-c(1,2)]
  observations <- x$observations
  number_of_observations <- length(observations)
  l <- lapply(
    1:number_of_observations,
    FUN = function(i)
      data.frame(
        from = pg$node,
        to = pg[[observations[i]]],
        label = observations[i]
      )
  )
  
  l <- do.call(rbind, l)
  l <- l[!is.na(l$to), ] # remove links to nowhere ('-' in pg)
  
  # creating the initial graph
  policy_graph <- graph.edgelist(as.matrix(l[, 1:2]))
  edge.attributes(policy_graph) <- list(label = l$label)
  edge.attributes(policy_graph) <- list(label = l$label)
  
  ### Note: the space helps with moving the id away from the pie cut.
  init <- rep(":   ", nrow(pg))
  init[x$solution$initial_pg_node] <- ": initial belief"
  
  V(policy_graph)$label <- paste0(pg$node, init, "\n", pg$action)
  
  # add belief proportions
  ### FIXME: Add gray for missing points instead of a uniform distribution
  if (belief) {
    pie_values <-
      lapply(
        seq_len(nrow(bp)),
        FUN = function(i)
          if (any(is.na(bp[i, ])))
            structure(rep(1 / ncol(bp), times = ncol(bp)), names = colnames(bp))
        else
          bp[i, ]
      )
    
    ### Set1 from Colorbrewer
    number_of_states <- length(x$states)
    col <- .get_colors_descrete(number_of_states, col)
    
    V(policy_graph)$shape <- "pie"
    V(policy_graph)$pie = pie_values
    V(policy_graph)$pie.color = list(col)
  }
  
  V(policy_graph)$size <- 40
  E(policy_graph)$arrow.size  <- .5
  
  policy_graph
}



# estimate central beliefs for each alpha vector (infinite horizon)
# sample points and then average over each alpha vector.
# TODO: finite horizon
# TODO: we could also calculate this with some linear algebra

#' @rdname plot_policy_graph
#' @param method sampling method used to estimate the belief. Methods "regular"
#'   and "random" call [sample_belief_space()]
#'   and method "simulate" calls [simulate_POMDP()]. Further arguments are passed on to these
#'   functions.
#' @export
estimate_belief_for_nodes <-
  function(x,
    method = c("regular", "random", "simulate"),
    ...) {
    method <- match.arg(method)
    
    alpha <- x$solution$alpha[[1]]
    
    belief_points <- switch(
      method,
      regular = sample_belief_space(x, method = "regular", ...),
      random = sample_belief_space(x, method = "random", ...),
      simulate = simulate_POMDP(x, visited_beliefs = TRUE, ...)
    )
    
    r <- reward(x, belief = belief_points)
    belief <- t(sapply(
      1:nrow(alpha),
      FUN = function(i)
        colMeans(r$belief[r$pg_node == i, , drop = FALSE])
    ))
    
    belief
  }
