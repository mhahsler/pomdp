#' Transition Graph
#'
#' Returns the transition model as an \pkg{igraph} object.
#'
#' The transition model of a POMDP/MDP is a Markov Chain. This function extracts the transition model as
#' an igraph object.
#'
#' @family POMDP
#' @family MDP
#'
#' @importFrom igraph graph_from_adjacency_matrix %>% E E<- V V<- add_layout_ as_data_frame as_tree graph_from_data_frame induced_subgraph norm_coords
#'
#' @param x object of class [POMDP] or [MDP].
#' @param action the name or id of an action or a set of actions. Bey default the transition model for all actions is returned.
#' @param episode,epoch  Episode or epoch used for time-dependent POMDPs. Epochs are internally converted to the episode using the model horizon.
#' @param state_col colors used to represent the states.
#' @param simplify_transitions logical; combine parallel transition arcs into a single arc.
#' @param remove_unavailable_actions logical; don't show arrows for unavailable actions.
#'
#' @returns returns the transition model as an igraph object.
#' @examples
#' data("Tiger")
#'
#' g <- transition_graph(Tiger)
#' g
#'
#' plot_transition_graph(Tiger)
#' plot_transition_graph(Tiger, vertex.size = 20, 
#'                       edge.label.cex = .5, edge.arrow.size = .5, margin = .5)
#' plot_transition_graph(Tiger, vertex.size = 60, 
#'                       edge.label = NA, edge.arrow.size = .5, 
#'                       layout = rbind(c(-1,0), c(+1,0)), rescale = FALSE)
#' 
#' ## Plot an individual graph for each actions and use a manual layout.
#' for (a in Tiger$actions) {
#'  plot_transition_graph(Tiger, action = a, 
#'                         layout = rbind(c(-1,0), c(+1,0)), rescale = FALSE,
#'                         main = paste("action:", a))
#' }
#'
#' ## Plot using the igraph library
#' library(igraph)
#' plot(g)
#'
#' # plot with a fixed layout and curved edges
#' plot(g,
#'  layout = rbind(c(-1, 0), c(1, 0)), rescale = FALSE,
#'  edge.curved = curve_multiple_directed(g, .8),
#'  edge.loop.angle = -pi / 4,
#'  vertex.size = 60
#'  )
#'
#' ## Use visNetwork (if installed)
#' if(require(visNetwork)) {
#'
#' g_vn <- toVisNetworkData(g)
#' nodes <- g_vn$nodes
#' edges <- g_vn$edges
#'
#' # add manual layout
#' nodes$x <- c(-1, 1) * 200
#' nodes$y <- 0
#'
#' visNetwork(nodes, edges)  %>%
#'   visNodes(physics = FALSE) %>%
#'   visEdges(smooth = list(type = "curvedCW", roundness = .6), arrows = "to")
#' }
#' @export
transition_graph <-
  function(x,
           action = NULL,
           episode = NULL,
           epoch = NULL,
           state_col = NULL,
           simplify_transitions = TRUE,
           remove_unavailable_actions = TRUE) {
    state_col <-
      colors_discrete(length(x$states), state_col)
    
    m <-
      transition_matrix(
        x,
        action = NULL,
        episode = episode,
        epoch = epoch,
        sparse = FALSE
      )
    
    if (is.null(action))
      action <- x$actions
    
    gs <- sapply(
      action,
      FUN = function(a) {
        g <-
          graph_from_adjacency_matrix(m[[a]], mode = "directed",  weighted = TRUE)
        E(g)$label <- a
        df <- as_data_frame(g)
        
        # remove unavailable actions
        if (remove_unavailable_actions) {
          available <- sapply(seq_len(nrow(df)), FUN = function(i) 
            all(reward_val(x, action = df$label[i], start.state = df$from[i], end.state = df$to[i]) != - Inf))
          df <- df[available, , drop = FALSE] 
        }
        df
      }, simplify = FALSE
    )
    
    g <-  graph_from_data_frame(do.call(rbind, gs))
    # make sure the vertices are in the same order as in the description
    g <- igraph::permute(g, match(V(g)$name, x$states))
    
     
    E(g)$label <- paste0(E(g)$label, ifelse(E(g)$weight != 1, paste0(" (", round(E(g)$weight, 2), ")"), ""))
    
    if (simplify_transitions)
      g <- igraph::simplify(
        g,
        edge.attr.comb = list(
          label = function(x)
            paste(x, collapse = "/\n"),
          "ignore"
        ),
        remove.loops = FALSE
      )
    
    if (!any(is.na(state_col)))
      V(g)$color <- state_col
    
    g
  }

#' @rdname transition_graph
#' @param main a main title for the plot.
#' @param ... further arguments are passed on to `igraph::plot.igraph()`.
#' @export
plot_transition_graph <- function(x,
                                  action = NULL,
                                  episode = NULL,
                                  epoch = NULL,
                                  state_col = NULL,
                                  simplify_transitions = TRUE,
                                  main = NULL,
                                  ...) {
  g <- transition_graph(
    x,
    action = action,
    episode = episode,
    epoch = epoch,
    state_col = state_col,
    simplify_transitions = simplify_transitions
  )
  
  if (is.null(main))
    main <- paste("Transition Graph:", x$name)
  
  plot(
    g,
    edge.curved = curve_multiple_directed(g, .8),
    edge.loop.angle = -pi / 4, main = main,
    ...
  )
  
}
