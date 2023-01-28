


#' @include plot_policy_graph.R
#' @rdname plot_policy_graph
#' @export
policy_graph <-
  function(x,
    belief = NULL,
    show_belief = TRUE,
    col = NULL,
    simplify_observations = FALSE,
    ...) {
    is_solved_POMDP(x, stop = TRUE)
    
    if (sum(x$horizon) < 2L)
      stop("No policy graph available for problems with a horizon < 2.")
    
    if (!is_converged_POMDP(x))
      policy_graph <- policy_graph_unconverged(x,
        belief,
        show_belief = show_belief,
        col = col,
        ...)
    else
      policy_graph <- policy_graph_converged(x,
        belief,
        show_belief = show_belief,
        col = col,
        ...)
    
    if (simplify_observations)
      policy_graph <-
        igraph::simplify(
          policy_graph,
          edge.attr.comb = list(
            label = function(x)
              paste(x, collapse = "/\n"),
            "ignore"
          ),
          remove.loops = FALSE
        )
    
    policy_graph
  }

policy_graph_converged <-
  function(x,
    belief = NULL,
    show_belief = TRUE,
    col = NULL,
    ...) {
    ### this is for sarsop...
    if (ncol(x$solution$pg[[1L]]) <= 2L) {
      pg_complete <- .infer_policy_graph_converged(x, ...)
      x$solution$central_belief <- pg_complete$central_belief
      x$solution$pg <- pg_complete$pg
    }
    
    # create policy graph and belief proportions (average belief for each alpha vector)
    pg <- x$solution$pg[[1L]]
    
    if (show_belief) {
      if (!is.null(x$solution$central_belief))
        bp <- x$solution$central_belief[[1L]]
      else
        bp <-
          estimate_belief_for_nodes(x, belief = belief, ...)[[1L]]
      
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
            bp[i, ]
        )
      
      ### Set1 from Colorbrewer
      col <- .get_colors_descrete(length(x$states), col)
      
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
        vertex_attr(policy_graph, "pie.color") <- list(col)
    }
    
    # visuals
    edge_attr(policy_graph, "arrow.size")  <- .5
    vertex_attr(policy_graph, "size") <- 100 / nrow(pg) ^ .5
    
    policy_graph
  }

policy_graph_unconverged <-
  function(x,
    belief = NULL,
    show_belief = TRUE,
    col = NULL,
    unreachable_nodes = FALSE,
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
    if (!unreachable_nodes) {
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
    l <- l[!is.na(l$to), ] # remove links to nowhere ('-' in pg)
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
      bp <- estimate_belief_for_nodes(x, belief = belief, ...)
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
            bp[i, ]
        )
      pie_values <- pie_values[m]
      
      ### Set1 from Colorbrewer
      number_of_states <- length(x$states)
      col <- .get_colors_descrete(number_of_states, col)
      
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
          rep(list(col), times = num_nodes)
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
.infer_policy_graph_converged <- function(model, ...) {
  if (!is.null(model$solution) && length(model$solution$pg) != 1L)
    stop("policy graph inference is currently only available for converges solutions!")
  
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
          update_belief(model, belief = central_beliefs[i, ], action = pg$action[i])
        )$pg_node
    ))
  colnames(pg_to) <- model$observations
  
  pg <- cbind(pg, pg_to)
  
  list(central_beliefs = list(central_beliefs),
    pg = list(pg))
}


### TODO: find the policy graph edges between episodes...
.infer_policy_graph_unconverged <- function(model, ...) {
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
          update_belief(model, belief = central_beliefs[i, ], action = pg$action[i])
        )$pg_node
    ))
  colnames(pg_to) <- model$observations
  
  pg <- cbind(pg, pg_to)
  
  list(central_beliefs = list(central_beliefs),
    pg = list(pg))
}