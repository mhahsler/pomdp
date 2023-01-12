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
#'   vertex.label = V(pg)$label,
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
policy_graph <- function(x, belief = NULL, show_belief = TRUE, col = NULL, ...) {
  is_solved_POMDP(x, stop = TRUE)
  
  if (!is_converged_POMDP(x))
    policy_graph_unconverged(x, belief, show_belief = show_belief, col = col, ...)
  else
    policy_graph_converged(x, belief, show_belief = show_belief, col = col, ...)
}
  
policy_graph_converged <- function(x, belief = NULL, show_belief = TRUE, col = NULL, ...) { 
   
  ### this is for sarsop...
  if (ncol(x$solution$pg[[1]]) <= 2) {
    pg_complete <- .add_policy_graph(x, ...)
    x$solution$central_belief <- pg_complete$central_belief
    x$solution$pg <- pg_complete$pg
  }
    
  # create policy graph and belief proportions (average belief for each alpha vector)
  pg <- x$solution$pg[[1]]
  
  if (show_belief) {
    if (!is.null(x$solution$central_belief))
      bp <- x$solution$central_belief[[1]]
    else
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
  
  l <- lapply(
    seq_along(observations),
    FUN = function(i)
      data.frame(
        from = pg$node,
        to = pg[[observations[i]]],
        label = observations[i]
      )
  )
  
  l <- do.call(rbind, l)
  l <- l[!is.na(l$to), ] # remove links to nowhere ('-' in pg)
  
  # creating graph
  policy_graph <- graph.edgelist(as.matrix(l[, 1:2]))
  edge.attributes(policy_graph) <- list(label = l$label)
  
  # mark the node for the initial belief
  if (is.null(belief))
    initial_pg_node <-  x$solution$initial_pg_node
  else
    initial_pg_node <- reward_node_action(x, belief = belief)$pg_node
    
  ### Note: the space helps with moving the id away from the pie cut.
  init <- rep(":   ", nrow(pg))
  init[initial_pg_node] <- ": initial belief"
  
  V(policy_graph)$label <- paste0(pg$node, init, "\n", pg$action)
  
  # add belief proportions
  ### FIXME: Add gray for missing points instead of a uniform distribution
  if (show_belief) {
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
    col <- .get_colors_descrete(length(x$states), col)
    
    V(policy_graph)$shape <- "pie"
    V(policy_graph)$pie = pie_values
    V(policy_graph)$pie.color = list(col)
  }
  
  V(policy_graph)$size <- 40
  E(policy_graph)$arrow.size  <- .5
  
  policy_graph
}

policy_graph_unconverged <- function(x, belief = NULL, show_belief = TRUE, col = NULL, ...) {
  pg <- x$solution$pg
  
  if (ncol(pg[[1]]) <= 2)
    stop("Solution does not contain policy graph information. Use a different solver.")
  
  observations <- x$observations
  epochs <- length(pg)
  
  # add episode to the node ids
  for(i in seq(epochs)) pg[[i]] <- cbind(pg[[i]], epoch = i)
  pg <- do.call(rbind, pg)
  
  pg[["node"]] <- paste0(pg[["epoch"]], "-", pg[["node"]])
  for(o in observations) {
    pg[[o]] <- paste0(pg[["epoch"]] + 1L, "-", pg[[o]])
    
    ## these should be NA. Make sure they are
    pg[[o]][pg[["epoch"]] == epochs] <- NA
  }
  
  # mark the node for the initial belief
  if (is.null(belief))
    initial_pg_node <-  x$solution$initial_pg_node
  else
    initial_pg_node <- reward_node_action(x, belief = belief)$pg_node
  
  ## remove unreached nodes
  used <- paste0("1-", initial_pg_node)
  for(i in seq(epochs)) {
    used <- append(used, unlist(pg[pg[["epoch"]] == i & pg[["node"]] %in% used, observations]))
  }
  
  used <- pg[["node"]] %in% used
  pg <- pg[used,]
  num_nodes <- nrow(pg)
  
  #pg[["node"]] <- factor(pg[["node"]])
  #for(o in observations) 
  #  pg[[o]] <- factor(pg[[o]], levels = levels(pg[["node"]]))
  
  
  
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
        from = pg[["node"]],
        to = pg[[observations[i]]],
        label = observations[i]
      )
  )
  
  l <- do.call(rbind, l)
  l <- l[!is.na(l$to), ] # remove links to nowhere ('-' in pg)
  num_edges <- nrow(l)
  
  # creating the initial graph
  policy_graph <- graph_from_edgelist(as.matrix(l[, 1:2]))
  edge_attr(policy_graph) <- list(label = l$label)
  
  ### Note: the space helps with moving the id away from the pie cut.
  #init <- rep(":   ", nrow(pg))
  #init[1] <- ": initial belief"
  
  m <- match(vertex_attr(policy_graph)$name, pg[["node"]]) 
  vertex_attr(policy_graph)$name <- paste0(vertex_attr(policy_graph)$name, ":\n", pg[["action"]][m])
  
  # add belief proportions
  ### FIXME: Add gray for missing points instead of a uniform distribution
  if (show_belief) {
    bp <-
      lapply(
        seq(epochs),
        FUN = function(e)
          estimate_belief_for_nodes(x, epoch = e, ...)
      )
    bp <- do.call(rbind, bp)
    bp <- bp[used, ]
    
    # missing belief points?
    missing_bp <- which(apply(is.na(bp), MARGIN = 1, any))
    if (length(missing_bp) > 0)
      warning(
        "No belief points sampled for policy graph node(s): ",
        paste(missing_bp, collapse = ", "),
        ". Increase the number for parameter belief (number of sampled points)."
      )
    
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
    
    vertex_attr(policy_graph)$shape <- rep("pie", times = num_nodes)
    vertex_attr(policy_graph)$pie = pie_values[m]
    vertex_attr(policy_graph)$pie.color = rep(list(col), times = num_nodes)
  }
  
  vertex_attr(policy_graph)$size <- rep(40, times = num_nodes)
  edge_attr(policy_graph)$arrow.size  <- rep(.5, times = num_edges)
  policy_graph <- add_layout_(policy_graph, as_tree()) 
  
  policy_graph
}


# estimate central beliefs for each alpha vector (infinite horizon)
# sample points and then average over each alpha vector.
# TODO: we could also calculate this with some linear algebra

#' @rdname policy_graph
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
    igraph = .plot.igraph(x, belief, show_belief = show_belief, legend = legend, col = col, ...),
    visNetwork = .plot.visNetwork(x, belief, show_belief = show_belief, legend = legend, col = col, ...)
  )
}


.plot.igraph <-
  function(x, belief = NULL, show_belief, legend, col, edge.curved = NULL, ...) {
    pg <- policy_graph(x, belief, show_belief = show_belief, col = col, ...)
    
    if (is.null(edge.curved))
      edge.curved <- .curve_multiple_directed(pg)
    
    plot.igraph(pg, edge.curved = edge.curved, ...)
    
    if (legend && show_belief && !is.null(V(pg)$pie)) {
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



### returns a list with central_beliefs and a completed pg 
### this currently only works for converged solutions
.add_policy_graph <- function(model, ...) {
  if (!is.null(model$solution) && length(model$solution$pg) != 1L)
    stop("policy graph inference is currently only available for converges solutions!")
  
  # find central beliefs and use them to create the policy graph
  central_beliefs <- estimate_belief_for_nodes(model, ...)
  if (nrow(central_beliefs) < nrow(model$solution$pg[[1]]))
    stop("Unable to estimate all central beliefs.")
  
  pg <- model$solution$pg[[1]]
  pg_to <-
    t(sapply(
      seq_len(nrow(central_beliefs)),
      FUN = function(i)
        reward_node_action(
          model,
          update_belief(model, belief = central_beliefs[i, ], action = pg$action[i])
        )$pg_node
    ))
  colnames(pg_to) <- model$observations
  
  pg <- cbind(pg, pg_to)
  
  list(central_beliefs = list(central_beliefs),
    pg = list(pg))
}
