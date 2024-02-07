#' Helper Functions for Gridworld MDPs
#'
#' Helper functions for gridworld MDPs to convert between state names and
#' gridworld positions, and for visualizing policies.
#'
#' Gridworlds are implemented with state names `s(row,col)`, where
#' `row` and `col` are locations in the matrix representing the gridworld.
#' The actions are `"up"`, `"right"`,  `"down"`, and  `"left"`. 
#'
#' `gridworld_init()` initializes a new gridworld creating a matrix
#' of states with the given dimensions. Other action names
#' can be specified, but they must have the same effects in the same order 
#' as above. Unreachable states (walls) and absorbing state can be defined.
#' Tis information is used to create a default transition function.
#' 
#' Several other helper functions
#' are provided.
#'
#' @name gridworld
#' @aliases gridworld
#' @family gridworld
#' @examples
#' # Define a gridworld: The Dyna Maze from Chapter 8 in the RL book
#' gw <- gridworld_init(c(6,9), 
#'                 unreachable_states = c("s(2,3)", "s(3,3)", "s(4,3)",
#'                                        "s(5,6)", 
#'                                        "s(1,8)", "s(2,8)", "s(3,8)"),
#'                 absorbing_states = "s(1,9)",
#'                 labels = list("s(1,9)" = "G",
#'                               "s(3,1)" = "S"))
#' gw
#' 
#' # display the state labels in the gridworld
#' gridworld_matrix(gw)
#'
#' # regular moves in the gridworld
#' gw$T("right", "s(1,1)", "s(1,2)")
#' gw$T("right", "s(2,2)", "s(2,3)")  ### we cannot move into an unreachable state
#' gw$T("right", "s(2,2)", "s(2,2)")  ### but the agent stays in place
#' 
#' # convert between state names and row/column indices
#' gridworld_s2rc("s(1,1)")
#' gridworld_rc2s(c(1,1))
#'
#' Dyna_maze <- MDP(
#'   states = gw$states,
#'   actions = gw$actions,
#'   transition_prob = gw$T,
#'   reward = rbind(R_(value = 0), R_(end.state = "s(1,9)", value = +1)),
#'   discount = 0.95,
#'   start = "s(3,1)",
#'   info = gw$info,
#'   name = "Dyna Maze"
#'   )
#'
#' Dyna_maze
#' 
#' gridworld_plot_transition_graph(Dyna_maze, 
#'     vertex.label = NA, vertex.size = 20)
#'
#' sol <- solve_MDP(Dyna_maze)
#' gridworld_plot_policy(sol)
#'
#'
#' # Use the existing Maze gridworld 
#' data(Maze)
#'
#' # show the layout
#' gridworld_matrix(Maze)
#' gridworld_matrix(Maze, what = "labels")
#'
#' # visualize the transition graph
#' gridworld_plot_transition_graph(Maze)
#'
#' # translate between state labels and gridworld locations
#' gridworld_rc2s(c(1,1))
#'
#' # for solved MDPs we can look at state values and policy actions
#' sol <- solve_MDP(Maze)
#' policy(sol)
#' gridworld_matrix(sol, what = "values")
#' gridworld_matrix(sol, what = "actions")
#'
#' # plot the solved gridworld
#' gridworld_plot_policy(sol)
#' gridworld_plot_policy(sol, arrows = FALSE)
#' @param dim vector of length two with the x and y extent of the gridworld.
#' @param A vector with four action labels that move the agent up, right, down,
#'   and left.
#' @param unreachable_states a vector with state labels for unreachable states. 
#'     These states will be excluded. 
#' @param absorbing_states a vector with state labels for absorbing states. 
#' @export
gridworld_init <- function(dim, A =  c("up", "right", "down", "left"), 
                           unreachable_states = NULL,
                           absorbing_states = NULL,
                           labels = NULL) {
  S <- as.vector(outer(
    seq_len(dim[1]),
    seq_len(dim[2]),
    FUN = function(x, y)
      paste0("s(", x, ",", y, ")")
  ))

  if (!is.null(unreachable_states))
    S <- setdiff(S, unreachable_states)
  
  T <- function(action, start.state = NULL, end.state = NULL) {
    ai <- pmatch(action, A)
    if (is.na(ai))
      stop("Unknown action", action)
    
    if (!is.null(absorbing_states) && start.state %in% absorbing_states)
      return(as.integer(end.state == start.state))
    
    rc <- gridworld_s2rc(start.state)
    rc <- switch (
      ai,
      rc + c(-1,  0),          ### up
      rc + c(0,+1),            ### right
      rc + c(+1,  0),          ### down
      rc + c(0,-1),            ### left
    )
   
    es <-  gridworld_rc2s(rc)
    if (!(es %in% S))
      es <- start.state
    as.integer(es == end.state)
  }
  
  list(
    states = S,
    actions = A,
    T = T,
    info = list(gridworld_dim = dim, gridworld_labels = labels)
  )
}

#' @rdname gridworld
#' @param model,x a solved gridworld MDP.
#' @param s a state label.
#' @param xy a vector of length two with the x and y coordinate of a
#'   state in the gridworld.
#' @export
gridworld_s2rc <- function(s) {
  xy <- as.integer(strsplit(s, "s\\(|,|\\)")[[1]][-1])
  if (length(xy) != 2 || any(is.na(xy)))
    stop("Malformed gridworld state label ",
         sQuote(s),
         ". Needs to be 's(x,y)'.")
  xy
}

#' @rdname gridworld
#' @export
gridworld_rc2s <- function(xy)
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
                                      levels = c("up", "right", "down", "left"), 
                                      labels = c("^", ">", "v", "<")))
    
    # plotting unicode characters is a problem.
                                      #labels = c("\U2191", "\U2192", "\U2193", "\U2190")))
      
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
    
    layout <- t(sapply(x$states, gridworld_s2rc))[, 2:1] *
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