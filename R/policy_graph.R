#' POMDP Policy Graphs
#'
#' The function creates a POMDP policy graph for converged POMDP solution and the
#' policy tree for a finite-horizon solution.
#' The graph is represented as an \pkg{igraph} object.
#'
#' Each policy graph node is represented by an alpha vector specifying a hyper plane segment. The convex hull of
#' the set of hyperplanes represents the the value function.
#' The policy specifies for each node an optimal action which is printed together with the node ID inside the node.
#' The arcs are labeled with observations.
#' Infinite-horizon converged solutions from a single policy graph. 
#' For finite-horizon solution a policy tree is produced.
#' The levels of the tree and the first number in the node label represent the epochs. 
#' 
#' The parameters `show_belief`, `remove_unreachable_nodes`, and `simplify_observations` are 
#' used by [plot_policy_graph()] (see there for details) to reduce clutter and make the visualization more readable. 
#' These options are disabled by default for `policy_graph()`.
#' @family policy
#'
#' @import igraph
#'
#' @param x object of class [POMDP] containing a solved and converged POMDP problem.
#' @param belief the initial belief is used to mark the initial belief state in the
#' grave of a converged solution and to identify the root node in a policy graph for a finite-horizon solution.
#' If `NULL` then the belief is taken from the model definition.
#' @param show_belief logical; show estimated belief proportions as a pie chart or color in each node?
#' @param state_col colors used to represent the belief over the states in each node. Only used if `show_belief` is `TRUE`.
#' @param simplify_observations combine parallel observation arcs into a single arc.
#' @param remove_unreachable_nodes logical; remove nodes that are not reachable from the start state? Currently only implemented for policy trees for unconverged finite-time horizon POMDPs.
#' @param ... parameters are passed on to [estimate_belief_for_nodes()].
#'
#' @returns returns the policy graph as an igraph object.
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
#' ### Policy trees for finite-horizon solutions
#' sol <- solve_POMDP(model = Tiger, horizon = 4, method = "incprune")
#'
#' policy_graph(sol)
#' plot_policy_graph(sol)
#' # Note: the first number in the node id is the epoch.
#' @export
policy_graph <-
  function(x,
    belief = NULL,
    show_belief = FALSE,
    state_col = NULL,
    simplify_observations = FALSE,
    remove_unreachable_nodes = FALSE,
    ...) {
    is_solved_POMDP(x, stop = TRUE)
    
    if (sum(x$horizon) < 2L)
      stop("No policy graph available for problems with a horizon < 2.")
    
    # infinite horizon and converged finite horizon problems have a policy graph
    # not converged finite horizon problems have a policy tree
    
    if (.has_policy_tree(x))
      policy_graph <- .policy_tree(
        x,
        belief,
        state_col = state_col,
        show_belief = show_belief,
        remove_unreachable_nodes =  remove_unreachable_nodes,
        ...
      )
    else
      policy_graph <- .policy_graph(
        x,
        belief,
        show_belief = show_belief,
        state_col = state_col,
        remove_unreachable_nodes = remove_unreachable_nodes,
        ...
      )
    
    if (simplify_observations)
      policy_graph <-
        igraph::simplify(
          policy_graph,
          edge.attr.comb = list(
            label = function(x)
              paste(x, collapse = "/\n"),
            observation = function(x)
              paste(x, collapse = "/\n"),
            arrow.size = function(x) mean(x),
            "ignore"
          ),
          remove.loops = FALSE
        )
    
    policy_graph
  }


.has_policy_tree <- function(x) !(is_converged_POMDP(x) || all(is.infinite(x$horizon)))


.policy_graph <-
  function(x,
    belief = NULL,
    show_belief = TRUE,
    state_col = NULL,
    remove_unreachable_nodes = FALSE,
    ...) {
    
    ## handle missing graph info
    if (ncol(x$solution$pg[[1L]]) <= 2L) {
      if (x$solution$method == "sarsop") {
        pg_complete <- .infer_policy_graph(x, ...)
        x$solution$central_belief <- pg_complete$central_belief
        x$solution$pg <- pg_complete$pg
      } else
        stop("Policy graph cannot be extracted!")
    }      
    
    # create policy graph and belief proportions (average belief for each alpha vector)
    pg <- x$solution$pg[[1L]]
    
    if (show_belief) {
      if (!is.null(x$solution$central_belief))
        bp <- x$solution$central_belief[[1L]]
      else
        bp <-
          suppressWarnings(estimate_belief_for_nodes(x, belief = belief, ...)[[1L]])
      
      # missing belief points?
      # missing_bp <- which(apply(is.na(bp), MARGIN = 1, any))
      # if (length(missing_bp) > 0)
      #   message(
      #     "policy_graph: No belief points available/sampled for policy graph node(s): ",
      #     paste(missing_bp, collapse = ", ")
      #   )
    }
    
    # producing a list containing arcs
    l <- list()
    list_of_arcs <- NULL
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
    l <- l[!is.na(l$to),] # remove links to nowhere ('-' in pg)
    
    # creating graph
    policy_graph <- graph.edgelist(as.matrix(l[, 1:2]))
    edge.attributes(policy_graph) <-
      list(label = l$label, observation = l$label)
    
    # mark the node for the initial belief
    if (is.null(belief))
      initial_pg_node <-  x$solution$initial_pg_node
    else
      initial_pg_node <-
      reward_node_action(x, belief = belief)$pg_node
    
    ### Note: the space helps with moving the id away from the pie cut.
    init <- rep("   ", nrow(pg))
    init[initial_pg_node] <- " - initial belief"
    
    vertex_attr(policy_graph, "label") <-
      paste0(pg$node, init, "\n", pg$action)
    
    vertex_attr(policy_graph, "id") <- pg$node
    vertex_attr(policy_graph, "action") <- as.character(pg$action)
    #vertex_attr(policy_graph, "alpha") <-lapply(seq_len(nrow(x$solution$alpha[[1L]])), FUN = function(i) x$solution$alpha[[1L]][i, ])
    
    # add belief proportions
    ### FIXME: Add gray for missing points instead of a uniform distribution
    if (show_belief) {
      pie_values <-
        lapply(
          seq_len(nrow(bp)),
          FUN = function(i)
            bp[i,]
        )
      
      ### Set1 from Colorbrewer
      state_col <-
        colors_discrete(length(x$states), state_col)
      
      # plot unknown beliefs as white circles
      vertex_attr(policy_graph, "shape") <-
        sapply(
          pie_values,
          FUN = function(i)
            if (any(is.na(i)))
              "circle"
          else
            "pie"
        )
      vertex_attr(policy_graph, "color") <- "white"
        vertex_attr(policy_graph, "pie") <- pie_values
        vertex_attr(policy_graph, "pie.color") <- list(state_col)
    }
    
    # visuals
    edge_attr(policy_graph, "arrow.size")  <- .5
    vertex_attr(policy_graph, "size") <- 100 / nrow(pg) ^ .5
    
    if (remove_unreachable_nodes)
      policy_graph <- igraph::delete_vertices(policy_graph, setdiff(seq_along(V(policy_graph)), 
        igraph::dfs(policy_graph, root = initial_pg_node, unreach = FALSE)$order))

    policy_graph
  }


.policy_tree <-
  function(x,
    belief = NULL,
    show_belief = TRUE,
    state_col = NULL,
    remove_unreachable_nodes = FALSE,
    ...) {
    pg <- x$solution$pg
    
    if (ncol(pg[[1]]) <= 2)
      stop("Solution does not contain policy graph information. Use a different solver.")
    
    observations <- x$observations
    epochs <- length(pg)
    
    # add episode to the node ids
    for (i in seq(epochs))
      pg[[i]] <- cbind(pg[[i]], epoch = i)
    pg <- do.call(rbind, pg)
    
    pg[["node"]] <- paste0(pg[["epoch"]], "-", pg[["node"]])
    for (o in observations) {
      pg[[o]] <- paste0(pg[["epoch"]] + 1L, "-", pg[[o]])
      
      ## these should be NA. Make sure they are
      pg[[o]][pg[["epoch"]] == epochs] <- NA
    }
    
    # mark the node for the initial belief
    if (is.null(belief))
      initial_pg_node <-  x$solution$initial_pg_node
    else
      initial_pg_node <-
      reward_node_action(x, belief = belief)$pg_node
    
    ## remove unreached nodes
    if (remove_unreachable_nodes) {
      used <- paste0("1-", initial_pg_node)
      for (i in seq(epochs)) {
        used <-
          append(used, unlist(pg[pg[["epoch"]] == i &
              pg[["node"]] %in% used, observations]))
      }
      
      used <- pg[["node"]] %in% used
      pg <- pg[used, , drop = FALSE]
    } else
      used <- seq_len(nrow(pg))
    
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
    l <- l[!is.na(l$to),] # remove links to nowhere ('-' in pg)
    num_edges <- nrow(l)
    
    # creating the initial graph
    policy_graph <- graph_from_edgelist(as.matrix(l[, 1:2]))
    edge_attr(policy_graph) <-
      list(label = l$label, observation = l$label)
    
    ### Note: the space helps with moving the id away from the pie cut.
    #init <- rep(":   ", nrow(pg))
    #init[1] <- ": initial belief"
    
    m <- match(vertex_attr(policy_graph, "name"), pg[["node"]])
    vertex_attr(policy_graph, "name") <-
      paste0(vertex_attr(policy_graph, "name"), "\n", pg[["action"]][m])
    
    vertex_attr(policy_graph, "id") <- pg[["node"]][m]
    vertex_attr(policy_graph, "epoch") <- pg[["epoch"]][m]
    vertex_attr(policy_graph, "action") <-
      as.character(pg[["action"]][m])
    
    # add belief proportions
    ### FIXME: Add gray for missing points instead of a uniform distribution
    if (show_belief) {
      bp <- suppressWarnings(estimate_belief_for_nodes(x, belief = belief, ...))
      bp <- do.call(rbind, bp)
      bp <- bp[used, , drop = FALSE]
      
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
            bp[i,]
        )
      pie_values <- pie_values[m]
      
      ### Set1 from Colorbrewer
      number_of_states <- length(x$states)
      state_col <-
        colors_discrete(number_of_states, state_col)
      
      vertex_attr(policy_graph, "shape") <-
        sapply(
          pie_values,
          FUN = function(i)
            if (any(is.na(i)))
              "circle"
          else
            "pie"
        )
      vertex_attr(policy_graph, "color") <- "white"
        vertex_attr(policy_graph, "pie") <- pie_values
        vertex_attr(policy_graph, "pie.color") <-
          rep(list(state_col), times = num_nodes)
    }
    
    # visuals
    edge_attr(policy_graph, "arrow.size")  <- .5
    vertex_attr(policy_graph, "size") <- 100 / nrow(pg) ^ .5
    
    policy_graph <- add_layout_(policy_graph, as_tree())
    
    policy_graph
  }


### This is used to infer a policy graph for SARSOP
### returns a list with central_beliefs and a completed pg
### this currently only works for converged solutions
.infer_policy_graph <- function(model, ...) {
  if (!is.null(model$solution) && length(model$solution$pg) != 1L)
    stop("Policy graph inference is currently only available for converges solutions!")
  
  # find central beliefs and use them to create the policy graph
  central_beliefs <- estimate_belief_for_nodes(model, ...)[[1L]]
  if (nrow(central_beliefs) < nrow(model$solution$pg[[1L]]))
    stop("Unable to estimate all central beliefs.")
  
  pg <- model$solution$pg[[1L]]
  pg_to <-
    t(sapply(
      seq_len(nrow(central_beliefs)),
      FUN = function(i)
        reward_node_action(
          model,
          update_belief(model, belief = central_beliefs[i,], action = pg$action[i])
        )$pg_node
    ))
  colnames(pg_to) <- model$observations
  
  pg <- cbind(pg, pg_to)
  
  list(central_beliefs = list(central_beliefs),
    pg = list(pg))
}


### TODO: find the policy graph edges between episodes...
.infer_policy_tree <- function(model, ...) {
  stop("TODO!!!")
  
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
          update_belief(model, belief = central_beliefs[i,], action = pg$action[i])
        )$pg_node
    ))
  colnames(pg_to) <- model$observations
  
  pg <- cbind(pg, pg_to)
  
  list(central_beliefs = list(central_beliefs),
    pg = list(pg))
}