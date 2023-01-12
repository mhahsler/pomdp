#' Estimate the Belief for Policy Graph Nodes
#'
#' Estimate a belief for each alpha vector which represents a node in the policy graph.
#'
#' `estimate_belief_for_nodes()` can estimate the belief in several ways:
#' 1. **Follow trajectories** till all policy graph nodes have been visited and return the encountered belief.
#'   If some nodes are not reachable from the initial belief, then a NAs will be returned with a warning.
#' 2. **Sample a large set** of possible belief points, assigning them to the segments and then averaging
#'   the belief over the points assigned to each segment. This will return a central belief for the node.
#'   Additional parameters like `method` and the sample size `n` are passed on to [sample_belief_space()].
#'   If no belief point is generated for a segment, then a
#'   warning is produced. In this case, the number of sampled points can be increased.
#'
#' @family policy
#'
#' @param x object of class [POMDP] containing a solved and converged POMDP problem.
#' @param epoch estimate the belief for nodes in this epoch. Use 1 for converged policies.
#' @param method character string specifying the estimation method.
#' @param ... parameters are passed on to `sample_belief_space()`.
#'
#' @returns
#' returns a matrix with a belief for each policy graph node.
#'
#' @examples
#' data("Tiger")
#'
#' ## policy graphs for converged solutions
#' sol <- solve_POMDP(model = Tiger)
#' sol
#'
#' estimate_belief_for_nodes(sol)
#'
#' estimate_belief_for_nodes(sol, method = "random")
#' @export
estimate_belief_for_nodes <-
  function(x,
    method = "auto",
    epoch = 1,
    ...) {
    method <-
      match.arg(method,
        choices = c("auto", "random", "regular", "vertices", "trajectories"))
    
    is_solved_POMDP(x, stop = TRUE)
    
    .aggregate_beliefs <- function(belief_points, alpha) {
      r <- .rew(belief_points, alpha)
      ind <- split(seq_len(nrow(belief_points)), f = factor(r$pg_node, levels = seq_len(nrow(alpha))))
      t(sapply(
        ind,
        FUN = function(i)
          colMeans(belief_points[i, , drop = FALSE])
      ))
    }
   
    belief <- NULL
    
    # default checks if the solver already provides belief states, if not it uses trajectories
    if (method == "auto")
      if (!is.null(x$solution$belief_points_solver))
        belief <-
      .aggregate_beliefs(x$solution$belief_points_solver, x$solution$alpha[[epoch]])
    else
      method <- "trajectories"
    
    if (method == "trajectories")
      if (is_converged_POMDP(x))
        belief <-
      .estimate_belief_for_nodes_trajectories(x, epoch, ...)
    else
      method < "random"
    
    if (is.null(belief))
      belief <-
      .aggregate_beliefs(sample_belief_space(x, method = method, ...),
        x$solution$alpha[[epoch]])
    
    if (nrow(belief) < nrow(x$solution$pg[[epoch]]))
      warning("Not enough points were sampled to estimate beliefs for all policy graph nodes. Some may be redundant. Increasing n may help for sampling.")
    
    belief
  }


## TODO: Estimate belief using trajectories...

## find a belief state for each alpha vector (we may need to remove redundant vectors first)
## Follow trajectories till we have all belief states
.estimate_belief_for_nodes_trajectories <-
  function(x,
    epoch = 1,
    ...) {
    is_converged_POMDP(x, stop = TRUE)
    
    pg <- x$solution$pg[[epoch]]
    found <- logical(nrow(pg)) ### all are FALSE
    a_belief <-
      matrix(NA, nrow = nrow(pg), ncol = length(x$states))
    
    # start with the initial belief
    belief_state <- start_vector(x)
    queue <- new.queue()
    
    
    while (!all(found)) {
      # add new found belief states
      rna <- reward_node_action(x, belief_state)
      new <- !found[rna$pg_node]
      new_states <- rna$pg_node[new]
      
      a_belief[new_states,] <-
        rbind(rna$belief)[new, , drop = FALSE]
      found[new_states] <- TRUE
      
      # go down the tree
      belief_states <- update_belief(x, belief_state)
      for (i in seq_len(nrow(belief_states)))
        enqueue(queue, belief_states[i, , drop = TRUE])
      
      
      if (queue.empty(queue)) {
        warning("Cannot find a belief for all alpha vector! Some NAs are returned.")
        break
      }
      
      belief_state <- dequeue(queue)
    }
    
    delete.queue(queue)
    
    colnames(a_belief) <- x$states
    a_belief
  }
