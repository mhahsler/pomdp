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
#' @import igraph
#'
#' @param x object of class [POMDP] or [MDP].
#' @param action the name or id of an action or a set of actions. Bey default the transition model for all actions is returned.
#' @param episode,epoch  Episode or epoch used for time-dependent POMDPs. Epochs are internally converted to the episode using the model horizon.
#' @param state_col colors used to represent the states.
#' @param simplify_transitions logical; combine parallel transition arcs into a single arc.
#'
#' @returns returns the transition model as an igraph object.
#' @examples
#' data("Tiger")
#'
#' g <- transition_graph(Tiger)
#' g
#'
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
#'  
#' ## Plot an individual graph for each actions
#' for (a in Tiger$actions) {
#'  g <- transition_graph(Tiger, action = a)
#' 
#'  plot(g,
#'   layout = rbind(c(-1, 0), c(1, 0)), rescale = FALSE,  
#'   edge.curved = curve_multiple_directed(g, .8),
#'   edge.loop.angle = cumsum(which_loop(g)) *  (-pi / 8),
#'   vertex.size = 60
#'  )
#' }
#' @export
transition_graph <- function(x, action = NULL, episode = NULL, epoch = NULL, state_col = NULL, simplify_transitions = TRUE) {
  state_col <-
    colors_discrete(length(x$states), state_col)
  
  m <- transition_matrix(x, action = action, episode = episode, epoch = epoch, sparse = FALSE, drop = FALSE)
  
  gs <- lapply(
    names(m),
    FUN = function(a) {
      g <-
        graph_from_adjacency_matrix(m[[a]], mode = "directed",  weighted = TRUE)
      E(g)$label <- a
      as_data_frame(g)
    }
  )
  
  g <-  graph_from_data_frame(do.call(rbind, gs))
  E(g)$label <- paste0(E(g)$label, " (", round(E(g)$weight, 2), ")")
  
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
  
  if(!any(is.na(state_col)))
    V(g)$color <- state_col
  
  g
}


