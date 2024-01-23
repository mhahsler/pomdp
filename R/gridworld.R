#' Helper Functions for Grid World MDPs
#'
#' Helper functions for grid world MDPs to convert between state names and
#' grid world positions, and for visualizing policies.
#'
#' Grid worlds are implemented with state names `s(x,y)`, where
#' `x` and `y` are state coordinates.
#'
#' `gridworld_init()` initializes a new grid world creating a matrix
#' of states with the given dimensions. Several other helper functions
#' are provided.
#'
#' @name gridworld
#' @aliases gridworld
#' @family gridworld
#' @examples
#' data(Maze)
#'
#' # show the layout
#' gridworld_matrix(Maze)
#' gridworld_matrix(Maze, what = "labels")
#'
#' # visualize the transition graph
#' gridworld_plot_transition_graph(Maze)
#'
#' # translate between state labels and grid world locations
#' gridworld_xy2s(c(1,1))
#'
#' # for solved MDPs we can look at state values and policy actions
#' sol <- solve_MDP(Maze)
#' policy(sol)
#' gridworld_matrix(sol, what = "values")
#' gridworld_matrix(sol, what = "actions")
#'
#' # plot the solved grid world
#' gridworld_plot_policy(sol)
#' gridworld_plot_policy(sol, arrows = FALSE)
#' @param dim vector of length two with the x and y extent of the grid world.
#' @export
gridworld_init <- function(dim) {
  S <- as.vector(outer(
    seq_len(dim[1]),
    seq_len(dim[2]),
    FUN = function(x, y)
      paste0("s(", x, ",", y, ")")
  ))
  A <- c("north", "east", "south", "west")
  list(
    states = S,
    actions = A,
    info = list(gridworld_dim = dim)
  )
}

#' @rdname gridworld
#' @param model,x a solved grid world MDP.
#' @param s a state label.
#' @param xy a vector of length two with the x and y coordinate of a
#'   state in the grid world.
#' @export
gridworld_s2xy <- function(s) {
  xy <- as.integer(strsplit(s, "s\\(|,|\\)")[[1]][-1])
  if (length(xy) != 2 || any(is.na(xy)))
    stop("Malformed grid world state label ",
         sQuote(s),
         ". Needs to be 's(x,y)'.")
  xy
}

#' @rdname gridworld
#' @export
gridworld_xy2s <- function(xy)
  paste0("s(", xy[1], ",", xy[2], ")")

#' @rdname gridworld
#' @param what What should be returned in the matrix. Options are:
#'  `"states"`, `"labels"`, `"values"`, `"actions"`, `"absorbing"`, and
#'  `"reachable"`.
#' @export
gridworld_matrix <- function(model, epoch = 1L, what = "states") {
  what <- match.arg(what,
                    c(
                      "states",
                      "labels",
                      "values",
                      "actions",
                      "absorbing",
                      "reachable"
                    ))
  nrows <- model$info$gridworld_dim[1]
  ncols <- model$info$gridworld_dim[2]
  
  all_states <- gridworld_init(dim = c(nrows, ncols))$states
  
  x <- switch(
    what,
    states = {
      l <- structure(rep(NA_character_, length(all_states)),
                     names = all_states)
      l[model$states] <- model$states
      l
    },
    
    labels = {
      l <- structure(rep("", length(all_states)),
                     names = all_states)
      l[!reachable_states(model)] <- "X"
      l[!(all_states %in% model$states)] <- "X"
      labels <- model$info$gridworld_labels
      l[names(labels)] <- unlist(labels)
      
      l
    },
    
    values = {
      l <- structure(rep(NA_real_, length(all_states)),
                     names = all_states)
      p <- policy(model, drop = FALSE)[[epoch]]
      l[p$state] <- p$U
      l
    },
    
    actions = {
      l <- structure(rep(NA_character_, length(all_states)),
                     names = all_states)
      p <- policy(model, drop = FALSE)[[epoch]]
      l[p$state] <- as.character(p$action)
      l
    },
    
    absorbing = {
      l <- structure(rep(TRUE, length(all_states)),
                     names = all_states)
      l[model$states] <- absorbing_states(model)
      l
    },
    reachable = {
      l <- structure(rep(FALSE, length(all_states)),
                     names = all_states)
      l[model$states] <- reachable_states(model)
      l
    }
  )
  
  matrix(x, nrow = nrows)
}

#' @rdname gridworld
#' @param epoch epoch for unconverged finite-horizon solutions.
#' @param arrows logical; show arrows instead of action names.
#' @param states logical; show state names.
#' @param labels logical; show state labels.
#' @param absorbing_state logical; show the value and the action for absorbing states.
#' @param main logical; main title.
#' @param cex expansion factor for the action.
#' @param offset move the state labels out of the way (in fractions of a character width).
#' @param ... further arguments are passed on to [graphics::image()].
#' @importFrom graphics image text box abline
#' @export
gridworld_plot_policy <-
  function(model,
           epoch = 1L,
           arrows = TRUE,
           states = TRUE,
           labels = TRUE,
           absorbing_state = FALSE,
           main = NULL,
           cex = 1.5,
           offset = .7,
           ...) {
    if (is.null(main))
      main <- model$name
    
    U <- gridworld_matrix(model, epoch = epoch, what = "value")
    
    if (!absorbing_state)
      U[gridworld_matrix(model, what = "absorbing")] <- NA
    
    image(t(U)[, rev(seq_len(nrow(U)))], main = main, axes = FALSE, ...)
    
    nrows <- model$info$gridworld_dim[1]
    ncols <- model$info$gridworld_dim[2]
    box()
    frac <- 1 / ((nrows - 1L) * 2)
    abline(h = (((1:nrows) * 2L) - 1L) * frac)
    frac <- 1 / ((ncols - 1L) * 2)
    abline(v = (((1:ncols) * 2L) - 1L) * frac)
    
    g <- expand.grid(y = (nrows - 1L):0 / (nrows - 1L),
                     x = 0:(ncols - 1L) / (ncols - 1L))
    
    g$state <-  as.vector(gridworld_matrix(model, what = "states"))
    g$labels <-  as.vector(gridworld_matrix(model, what = "labels"))
    g$actions <-
      as.vector(gridworld_matrix(model, epoch = epoch, what = "actions"))
    
    if (!absorbing_state)
      g$actions[gridworld_matrix(model, what = "absorbing")] <- NA
    
    if (states)
      text(g$x, g$y, g$state, pos = 3, offset = offset, cex = .8)
      #text(g$x, g$y, g$state, pos = 3, cex = .8)
    
    if (labels)
      text(g$x, g$y, g$label, pos = 1, offset = offset, cex = .8)
   
    if (arrows)
      g$actions <- as.character(factor(g$actions, 
                                      levels = c("north", "east", "south", "west"), 
                                      labels = c("\U2191", "\U2192", "\U2193", "\U2190")))
      
    text(g$x, g$y, g$actions, cex = cex)
  }

#' @rdname gridworld
#' @param hide_unreachable_states logical; do not show unreachable states. 
#' @param remove.loops logical; do not show transitions from a state back to itself. 
#' @param vertex.color,vertex.shape,vertex.size,edge.arrow.size see `igraph::igraph.plotting`.
#' @param margin a single number specifying the margin of the plot. Can be used if the 
#'   graph does not fit inside the plotting area.
#' @param main a main title for the plot. Defaults to the name of the problem.
#' @param ... further arguments are passed on to `igraph::plot.igraph()`.
#' @export
gridworld_plot_transition_graph <-
  function(x,
           hide_unreachable_states = TRUE,
           remove.loops = TRUE,
           vertex.color = "gray",
           vertex.shape = "square",
           vertex.size = 40,
           edge.arrow.size = .3,
           margin = .2,
           main = NULL,
           ...) {
    g <- transition_graph(x)
    
    layout <- t(sapply(x$states, gridworld_s2xy))[, 2:1] *
      cbind(rep(1, length(x$states)), -1)
    
    if (hide_unreachable_states) {
      reachable <- reachable_states(x)
      g <- induced_subgraph(g, V(g)[reachable])
      layout <- layout[reachable, ]
    }
    
    V(g)$color <- vertex.color
    V(g)$shape <- vertex.shape
    V(g)$size <- vertex.size
    
    if (remove.loops)
      g <- igraph::simplify(g, remove.loops = TRUE)
    
    asp <- x$info$gridworld_dim[2] / x$info$gridworld_dim[1]
    
    if (is.null(main))
      main <- x$name
    
    plot(
      g,
      layout = norm_coords(layout, xmin = -asp, xmax = asp),
      rescale = FALSE,
      xlim = c(-asp * (1 + margin), asp *  (1 + margin)),
      ylim = c(- (1 + margin),  (1 + margin)),
      edge.arrow.size = edge.arrow.size,
      main = main,
      ...
    )
    
    invisible(g)
  }