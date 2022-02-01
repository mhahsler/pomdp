#' Steward Russell's 3x4 Maze MDP
#' 
#' The 3x4 maze described in Chapter 17 of the the textbook: "Artificial Intelligence: A Modern Approach."
#'
#' The simple maze has the following layout:
#'
#' \preformatted{
#'     1234        Transition model:
#'    ######             .8 (action direction)
#'   3#   +#              ^
#'   2# # -#              |
#'   1#    #         .1 <-|-> .1
#'    ######
#' }
#'
#' We represent the maze states as a matrix with 3 rows (north/south) and 
#' 4 columns (east/west). The states are labeled `s_1` through `s_12` and are fully observable. 
#' The # (state `s_5`) in the middle of the maze is an obstruction and not reachable.  
#' Rewards are associated with transitions. The default reward (penalty) is -0.04.
#' Transitioning to + (state `s_12`) gives a reward of 1.0, transitioning to - (state `s_11`)
#' has a reward of -1.0. States `s_11` and `s_12` are terminal states.
#' 
#' Actions are movements (`n`, `s`, `e`, `w`). The actions are unreliable with a .8 chance
#' to move in the correct direction and a 0.1 chance to instead to move in a 
#' perpendicular direction leading to a stochastic transition model.
#' 
#' Note that the problem has reachable terminal states which leads to a proper policy
#' (that is guaranteed to reach a terminal state). This means that the solution also 
#' converges without discounting (`discount = 1`). 
#' @name Maze
#' @aliases Maze maze
#' @docType data
#' @format An object of class [MDP].
#' @keywords datasets
#' @references Russell, S. J. and Norvig, P., & Davis, E. (2021). Artificial intelligence: a modern approach. 4rd ed.
#' @examples
#' # The problem can be loaded using data(Maze).
#' 
#' # Here is the complete problem definition:
#'
#' S <- paste0("s_", seq_len(3 * 4))
#' s2rc <- function(s) {
#'   if(is.character(s)) s <- match(s, S)
#'   c((s - 1) %% 3 + 1, (s - 1) %/% 3 + 1)
#' }
#' rc2s <- function(rc) S[rc[1] + 3 * (rc[2] - 1)]
#' 
#' A <- c("n", "s", "e", "w")
#' 
#' T <- function(action, start.state, end.state) {
#'   action <- match.arg(action, choices = A)
#'   
#'   if (start.state %in% c('s_11', 's_12', 's_5')) {
#'     if (start.state == end.state) return(1)
#'     else return(0)
#'   }
#'
#'   if(action %in% c("n", "s")) error_direction <- c("e", "w")
#'   else error_direction <- c("n", "s")
#'   
#'   rc <- s2rc(start.state)
#'   delta <- list(n = c(+1, 0), s = c(-1, 0), 
#'                 e = c(0, +1), w = c(0, -1))
#'   P <- matrix(0, nrow = 3, ncol = 4)
#'   
#'   add_prob <- function(P, rc, a, value) {
#'     new_rc <- rc + delta[[a]]
#'     if (new_rc[1] > 3 || new_rc[1] < 1 || new_rc[2] > 4 || new_rc[2] < 1 
#'       || (new_rc[1] == 2 && new_rc[2]== 2))
#'       new_rc <- rc
#'     P[new_rc[1], new_rc[2]] <- P[new_rc[1], new_rc[2]] + value
#'     P
#'   }
#'  
#'  P <- add_prob(P, rc, action, .8)
#'  P <- add_prob(P, rc, error_direction[1], .1)
#'  P <- add_prob(P, rc, error_direction[2], .1)
#'  P[rbind(s2rc(end.state))]
#' }
#' 
#' T("n", "s_1", "s_2")
#' 
#' R <- rbind(
#'  R_(end.state   = '*',     value = -0.04),
#'  R_(end.state   = 's_11',  value = -1),
#'  R_(end.state   = 's_12',  value = +1),
#'  R_(start.state = 's_11',  value = 0),
#'  R_(start.state = 's_12',  value = 0),
#'  R_(start.state = 's_5',  value = 0)
#' )
#' 
#' 
#' Maze <- MDP(
#'  name = "Stuart Russell's 3x4 Maze",
#'  discount = 1,
#'  states = S,
#'  actions = A,
#'  transition_prob = T,
#'  reward = R
#' ) 
#' 
#' maze_solved <- solve_MDP(Maze, method = "value")
#' policy(maze_solved)
#' 
#' maze_POMDP <- MDP2POMDP(Maze)
#' maze_POMDP_solved <- solve_POMDP(maze_POMDP)
#' policy(maze_POMDP_solved) 
NULL

## save(Maze, file = "data/Maze.rda")