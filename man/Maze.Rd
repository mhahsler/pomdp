% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Maze.R
\docType{data}
\name{Maze}
\alias{Maze}
\alias{maze}
\title{Steward Russell's 4x3 Maze Gridworld MDP}
\format{
An object of class \link{MDP}.
}
\description{
The 4x3 maze is described in Chapter 17 of the textbook
"Artificial Intelligence: A Modern Approach" (AIMA).
}
\details{
The simple maze has the following layout:

\preformatted{
    1234           Transition model:
   ######             .8 (action direction)
  1#   +#              ^
  2# # -#              |
  3#S   #         .1 <-|-> .1
   ######
}

We represent the maze states as a gridworld matrix with 3 rows and
4 columns. The states are labeled \code{s(row, col)} representing the position in
the matrix.
The # (state \code{s(2,2)}) in the middle of the maze is an obstruction and not reachable.
Rewards are associated with transitions. The default reward (penalty) is -0.04.
The start state marked with \code{S} is \code{s(3,1)}.
Transitioning to \code{+} (state \code{s(1,4)}) gives a reward of +1.0,
transitioning to \code{-} (state \code{s_(2,4)})
has a reward of -1.0. Both these states are absorbing
(i.e., terminal) states.

Actions are movements (\code{up}, \code{right}, \code{down}, \code{left}). The actions are
unreliable with a .8 chance
to move in the correct direction and a 0.1 chance to instead to move in a
perpendicular direction leading to a stochastic transition model.

Note that the problem has reachable terminal states which leads to a proper policy
(that is guaranteed to reach a terminal state). This means that the solution also
converges without discounting (\code{discount = 1}).
}
\examples{
# The problem can be loaded using data(Maze).

# Here is the complete problem definition:
gw <- gridworld_init(dim = c(3, 4), unreachable_states = c("s(2,2)"))
gridworld_matrix(gw)

# the transition function is stochastic so we cannot use the standard
# gridworld gw$transition_prob() function
T <- function(action, start.state, end.state) {
  action <- match.arg(action, choices = gw$actions)
  
  # absorbing states
  if (start.state \%in\% c('s(1,4)', 's(2,4)')) {
    if (start.state == end.state) return(1)
    else return(0)
  }
  
  # actions are stochastic so we cannot use gw$trans_prob
  if(action \%in\% c("up", "down")) error_direction <- c("right", "left")
  else error_direction <- c("up", "down")
  
  rc <- gridworld_s2rc(start.state)
  delta <- list(up = c(-1, 0), 
                down = c(+1, 0),
                right = c(0, +1), 
                left = c(0, -1))
  P <- matrix(0, nrow = 3, ncol = 4)

  add_prob <- function(P, rc, a, value) {
    new_rc <- rc + delta[[a]]
    if (!(gridworld_rc2s(new_rc) \%in\% gw$states))
      new_rc <- rc
    P[new_rc[1], new_rc[2]] <- P[new_rc[1], new_rc[2]] + value
    P
  }

  P <- add_prob(P, rc, action, .8)
  P <- add_prob(P, rc, error_direction[1], .1)
  P <- add_prob(P, rc, error_direction[2], .1)
  P[rbind(gridworld_s2rc(end.state))]
}

T("up", "s(3,1)", "s(2,1)")

R <- rbind(
 R_(end.state   = NA,     value = -0.04),
 R_(end.state   = 's(2,4)',  value = -1),
 R_(end.state   = 's(1,4)',  value = +1),
 R_(start.state = 's(2,4)',  value = 0),
 R_(start.state = 's(1,4)',  value = 0)
)


Maze <- MDP(
 name = "Stuart Russell's 3x4 Maze",
 discount = 1,
 horizon = Inf,
 states = gw$states,
 actions = gw$actions,
 start = "s(3,1)",
 transition_prob = T,
 reward = R,
 info = list(gridworld_dim = c(3, 4),
             gridworld_labels = list(
                "s(3,1)" = "Start",
                "s(2,4)" = "-1",
                "s(1,4)" = "Goal: +1"
                )
             )
)

Maze

str(Maze)

gridworld_matrix(Maze)
gridworld_matrix(Maze, what = "labels")

# find absorbing (terminal) states
which(absorbing_states(Maze))

maze_solved <- solve_MDP(Maze)
policy(maze_solved)

gridworld_matrix(maze_solved, what = "values")
gridworld_matrix(maze_solved, what = "actions")

gridworld_plot_policy(maze_solved)
}
\references{
Russell,9 S. J. and Norvig, P. (2020). Artificial Intelligence:
A modern approach. 4rd ed.
}
\seealso{
Other MDP_examples: 
\code{\link{Cliff_walking}},
\code{\link{DynaMaze}},
\code{\link{MDP}()},
\code{\link{Windy_gridworld}}

Other gridworld: 
\code{\link{Cliff_walking}},
\code{\link{DynaMaze}},
\code{\link{Windy_gridworld}},
\code{\link{gridworld}}
}
\concept{MDP_examples}
\concept{gridworld}
\keyword{datasets}
