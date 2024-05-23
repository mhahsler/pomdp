#' The Dyna Maze
#'
#' The Dyna Maze from Chapter 8 of the textbook 
#' "Reinforcement Learning: An Introduction."
#'
#' The simple 6x9 maze with a few walls. 
#' @name Maze
#' @aliases Maze maze
#' @family MDP_examples
#' @family gridworld
#' @docType data
#' @format An object of class [MDP].
#' @keywords datasets
#' @family MDP_examples
#' @family gridworld
#' @references
#' Richard S. Sutton and Andrew G. Barto (2018). Reinforcement Learning: An Introduction
#' Second Edition, MIT Press, Cambridge, MA.
#' @examples
#' data(DynaMaze)
#' 
#' DynaMaze
#' 
#' gridworld_matrix(DynaMaze)
#' gridworld_matrix(DynaMaze, what = "labels")
#'
#' gridworld_plot_transition_graph(Dyna_maze)
NULL

