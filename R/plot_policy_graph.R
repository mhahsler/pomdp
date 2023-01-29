#' POMDP Policy Graphs
#'
#' The function creates and plots the POMDP policy graph for converged POMDP solution and the
#' policy tree for a finite-horizon solution.
#' The graph is represented as an \pkg{igraph} object.
#'
#' Each policy graph node is represented by an alpha vector specifying a hyper plane segment. The convex hull of
#' the set of hyperplanes represents the the value function.
#' The policy specifies for each node an optimal action which is printed together with the node ID inside the node.
#' The arcs are labeled with observations.
#'
#' Infinite-horizon converged solutions from a single policy graph. 
#' For finite-horizon solution a policy tree is produced.
#' The levels of the tree and the first number in the node label represent the epochs. 
#' 
#' For better visualization, we provide a few features:
#' 
#' * Show Belief: A pie chart (or the color) in each node can be used
#'   represent an example of the belief that the agent has if it is in this node.
#'   This can help with interpreting the policy graph. The belief is obtained by calling
#'   [estimate_belief_for_nodes()].
#' * Remove unreachable states: Many algorithms produce
#'   unused policy graph nodes which can be filtered to produce a smaller tree structure of actually used nodes.
#'   Non-converged policies depend on the initial belief and if an initial belief is
#'   specified, then different nodes will be filtered and the tree will look different.
#' * Simplify observations: In some cases, two observations can lead to the same node resulting in two parallel edges.
#'   These edges can be collapsed into one labels with the observations. 
#'
#' These improvements are used by default by `plot_policy_graph()` and can be disabled using parameters.
#'
#' `policy_graph()` can be used to get a [igraph] representation of the policy graph. Here, the improvements are 
#' disabled by default. The igraph object can be used for plotting, using in other packages and for saving to disk 
#' to work with the policy graph in external applications. 
#' @family policy
#'
#' @import igraph
#'
#' @param x object of class [POMDP] containing a solved and converged POMDP problem.
#' @param belief the initial belief is used to mark the initial belief state in the
#' grave of a converged solution and to identify the root node in a policy graph for a finite-horizon solution.
#' If `NULL` then the belief is taken from the model definition.
#' @param show_belief logical; show estimated belief proportions as a pie chart or color in each node?
#' @param belief_col colors used to represent the belief in each node. Only used if `show_belief` is `TRUE`.
#' @param legend logical; display a legend for colors used belief proportions?
#' @param engine The plotting engine to be used.
#' @param simplify_observations combine parallel observation arcs into a single arc.
#' @param remove_unreachable_nodes logical; remove nodes that are not reachable from the start state? Currently only implemented for policy trees for unconverged finite-time horizon POMDPs.
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
#' ### Policy graphs for converged solutions
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
#' plot_policy_graph(sol, layout = rbind(c(1,1), c(1,-1), c(0,0), c(-1,-1), c(-1,1)), margin = .2)
#' plot_policy_graph(sol,
#'   layout = rbind(c(1,0), c(.5,0), c(0,0), c(-.5,0), c(-1,0)), rescale = FALSE,
#'   vertex.size = 15, edge.curved = 2,
#'   main = "Tiger Problem")
#'
#' ## hide labels, beliefs and legend
#' plot_policy_graph(sol, show_belief = FALSE, edge.label = NA, vertex.label = NA, legend = FALSE)
#'
#' ## custom larger vertex labels (A, B, ...)
#' plot_policy_graph(sol,
#'   vertex.label = LETTERS[1:nrow(policy(sol)[[1]])],
#'   vertex.size = 60,
#'   vertex.label.cex = 2,
#'   edge.label.cex = .7,
#'   vertex.label.color = "white")
#'
#' ## plotting the igraph object directly
#' pg <- policy_graph(sol, show_belief = TRUE, 
#'   simplify_observations = TRUE, remove_unreachable_nodes = TRUE)
#'
#' ## (e.g., using a tree layout)
#' plot(pg, layout = layout_as_tree(pg, root = 3, mode = "out"))
#'
#' ## change labels (abbreviate observations and use only actions to label the vertices)
#' plot(pg,
#'   edge.label = abbreviate(E(pg)$label),
#'   vertex.label = V(pg)$action,
#'   vertex.size = 20)
#'
#' ## use action to color vertices (requires a graph without a belief pie chart) 
#' ##    and color edges to represent observations.
#' pg <- policy_graph(sol, show_belief = FALSE, 
#'   simplify_observations = TRUE, remove_unreachable_nodes = TRUE)
#' 
#' plot(pg,
#'   vertex.label = NA,
#'   vertex.color = factor(V(pg)$action),
#'   vertex.size = 20,
#'   edge.color = factor(E(pg)$observation),
#'   edge.curved = .1
#'   )
#' 
#' acts <- levels(factor(V(pg)$action))
#' legend("topright", legend = acts, title = "action",
#'   col = igraph::categorical_pal(length(acts)), pch = 15)
#' obs <- levels(factor(E(pg)$observation))
#' legend("bottomright", legend = obs, title = "observation",
#'   col = igraph::categorical_pal(length(obs)), lty = 1) 
#'
#' ## plot interactive graphs using the visNetwork library.
#' ## Note: the pie chart representation is not available, but colors are used instead.
#' plot_policy_graph(sol, engine = "visNetwork")
#'
#' ## add smooth edges and a layout (note, engine can be abbreviated)
#' plot_policy_graph(sol, engine = "visNetwork", layout = "layout_in_circle", smooth = TRUE)
#'
#'
#' ### Policy trees for finite-horizon solutions
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
#' plot_policy_graph(sol, edge.arrow.size = .1,
#'   vertex.label.cex = .5, edge.label.cex = .5)
#'
#' plot_policy_graph(sol, engine = "visNetwork")
#' @export
plot_policy_graph <- function(x,
  belief = NULL,
  show_belief = TRUE,
  belief_col = NULL,
  legend = TRUE,
  engine = c("igraph", "visNetwork"),
  simplify_observations = TRUE,
  remove_unreachable_nodes = TRUE,
  ...) {
  engine <- match.arg(engine)
  switch(
    engine,
    igraph = .plot.igraph(
      x,
      belief,
      show_belief = show_belief,
      belief_col = belief_col,
      legend = legend,
      simplify_observations = simplify_observations,
      remove_unreachable_nodes = remove_unreachable_nodes,
      ...
    ),
    visNetwork = .plot.visNetwork(
      x,
      belief,
      show_belief = show_belief,
      legend = legend,
      belief_col = belief_col,
      simplify_observations = simplify_observations,
      remove_unreachable_nodes = remove_unreachable_nodes,
      ...
    )
  )
}


.plot.igraph <-
  function(x,
    belief = NULL,
    show_belief,
    belief_col,
    legend,
    simplify_observations,
    remove_unreachable_nodes,
    edge.curved = NULL,
    ...) {
    pg <-
      policy_graph(
        x,
        belief,
        show_belief = show_belief,
        belief_col = belief_col,
        simplify_observations = simplify_observations,
        remove_unreachable_nodes = remove_unreachable_nodes,
        ...
      )
    
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

# plot policy graph using visNetwork

# Note: legend is not used right now!
.plot.visNetwork <-
  function(x,
    belief = NULL,
    show_belief = TRUE,
    belief_col = NULL,
    legend = NULL,
    simplify_observations,
    remove_unreachable_nodes,
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
      policy_graph(
        x,
        belief,
        show_belief = show_belief,
        belief_col = belief_col,
        simplify_observations = simplify_observations,
        remove_unreachable_nodes = remove_unreachable_nodes
      )
    
    ### add tooltip
    #V(pg)$title <- paste(htmltools::tags$b(V(pg)$label)
    if (!is.null(vertex_attr(pg, "epoch")))
      ep <-
      paste("<b>epoch:</b>",
        vertex_attr(pg, "epoch"),
        "<br>")
    else
      ep <- ""
    
    vertex_attr(pg, "title") <- paste(
      "<b>node id:</b>",
      vertex_attr(pg, "id"),
      "<br>",
      ep,
      "<b>action:</b>",
      vertex_attr(pg, "action"),
      "<p>",
      lapply(
        vertex_attr(pg, "pie"),
        FUN = function(b) {
          knitr::kable(cbind(belief = b), digits = 3, format = "html")
        }
      )
    )
    
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
          if (any(is.na(mix)))
            mix <- cbind(255, 255, 255)
          grDevices::rgb(mix, maxColorValue = 255)
        }
      )
      
      # mixing in hsv space
      #V(pg)$color <- sapply(seq(length(V(pg))), FUN = function(i)
      #  do.call(hsv, as.list(rgb2hsv(col2rgb(V(pg)$pie.color[[1]])) %*% V(pg)$pie[[i]])))
    }
    
    # tree layout needs flip.y
    if (layout == "layout_as_tree")
      visNetwork::visIgraph(
        pg,
        idToLabel = FALSE,
        layout = layout,
        smooth = smooth,
        flip.y = FALSE,
        ...
      ) %>%
      visNetwork::visOptions(
        highlightNearest = list(enabled = TRUE, degree = 0),
        nodesIdSelection = TRUE
      )
    else
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
