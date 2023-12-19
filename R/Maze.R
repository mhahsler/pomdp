#' Steward Russell's 4x3 Maze MDP
#' 
#' The 4x3 maze described in Chapter 17 of the the textbook: "Artificial Intelligence: A Modern Approach" (AIMA).
#'
#' The simple maze has the following layout:
#'
#' \preformatted{
#'     1234        Transition model:
#'    ######             .8 (action direction)
#'   3#   +#              ^
#'   2# # -#              |
#'   1#S   #         .1 <-|-> .1
#'    ######
#' }
#'
#' We represent the maze states as a matrix with 3 rows (up/down) and 
#' 4 columns (left/right). The states are labeled `s_1` through `s_12` 
#' (bottom-left to top right) and are fully observable. 
#' The # (state `s_5`) in the middle of the maze is an obstruction and not reachable.  
#' Rewards are associated with transitions. The default reward (penalty) is -0.04.
#' The start state marked with `S` is `s_1`.
#' Transitioning to `+` (state `s_12`) gives a reward of +1.0, transitioning to `-` (state `s_11`)
#' has a reward of -1.0. States `s_11` and `s_12` are terminal (absorbing) states.
#' 
#' Actions are movements (`up`, `down`, `left`, `right`). The actions are unreliable with a .8 chance
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
#' A <- c("up", "down", "left", "right")
#' 
#' T <- function(action, start.state, end.state) {
#'   action <- match.arg(action, choices = A)
#'   
#'   if (start.state %in% c('s_11', 's_12', 's_5')) {
#'     if (start.state == end.state) return(1)
#'     else return(0)
#'   }
#'
#'   if(action %in% c("up", "down")) error_direction <- c("right", "left")
#'   else error_direction <- c("up", "down")
#'   
#'   rc <- s2rc(start.state)
#'   delta <- list(up = c(+1, 0), down = c(-1, 0), 
#'                 right = c(0, +1), left = c(0, -1))
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
#' T("up", "s_1", "s_2")
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
#'  horizon = Inf,
#'  states = S,
#'  actions = A,
#'  start = 1,
#'  transition_prob = T,
#'  reward = R
#' ) 
#'
#' Maze
#' str(Maze) 
#'
#' # Layout with state names
#' matrix(Maze$states,nrow = 3, dimnames = list(1:3, 1:4))[3:1, ]
#' 
#' maze_solved <- solve_MDP(Maze, method = "value")
#' policy(maze_solved)
#' 
#' # show the utilities and optimal actions organized in the maze layout (like in the AIMA textbook)
#' matrix(policy(maze_solved)[[1]]$U, nrow = 3, dimnames = list(1:3, 1:4))[3:1, ]
#' matrix(policy(maze_solved)[[1]]$action, nrow = 3, dimnames = list(1:3, 1:4))[3:1, ]
#' 
#' # Note: the optimal actions for the states with a utility of 0 are artefacts and should be ignored. 
NULL

## save(Maze, file = "data/Maze.rda")
