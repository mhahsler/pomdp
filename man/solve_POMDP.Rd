\name{solve_POMDP}
\alias{solve_POMDP}
\alias{solve_POMDP_parameter}
\title{Solve a POMDP Problem}
\description{
This function utilizes the 'pomdp-solve' program (written in C) to 
use different solution methods [2] to
solve problems that are formulated as partially observable Markov decision processes (POMDPs) [1]. The result is a (close to) optimal policy.
}
\usage{
solve_POMDP(model, horizon = Inf, discount = NULL, terminal_values = NULL,
  method = "grid", parameter= NULL, verbose = FALSE)
solve_POMDP_parameter()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ a POMDP problem specification created with \code{\link{POMDP}}. Alternatively,
  a POMDP file or the URL for a POMDP file can be specified.}
  \item{method}{ string; one of the following solution methods: 
  \code{"grid"},
  \code{"enum"},
  \code{"twopass"},
  \code{"witness"}, or
  \code{"incprune"}. Details can be found in [1]. The default is \code{"grid"} implementing the finite grid method. The size of the grid can be set via \code{parameter = list(fg_points = 10)}.}
  \item{horizon}{ an integer with the number of iterations for finite horizon problems. If set to \code{Inf}, the algorithm continues running iterations till it converges to the infinite horizon solution.}
  \item{discount}{ discount factor in range [0, 1]. If \code{discount} is not \code{NULL}, then the discount factor specified in \code{model} will be overwritten.} 
  \item{terminal_values}{a vector with the terminal values for each state or a matrix specifying the terminal rewards via a terminal value function (e.g., the alpha component produced by \code{solve_POMDP}). }
  \item{parameter}{ a list with parameters passed on to the pomdp-solve program. }
  \item{verbose}{
logical, if set to \code{TRUE}, the function provides the output of the pomdp solver in the R console.
}
}
\details{
\code{solve_POMDP_parameter()} displays available solver parameter options.

Infinite-horizon POMDPs (\code{horizon = Inf}) converge to a single policy graph. Finite-horizon POMDPs result in a
policy tree of a depth equal to the smaller of the horizon or the number of epochs to convergence. The policy (and the associated value function) are stored in a list by level. The policy for the first epoch is stored as the first element.

The policy graph is a data frame where each row representing a policy graph node with an associated optimal action and
a list of node IDs to go to depending on the observation (specified as the column names). For the finite-horizon case, the observation specific node IDs refer to nodes in the next epoch creating a policy tree. 

The value function is stored as a matrix. Each row is associated with a node (row) in the policy graph and represents the coefficients (alpha vector) of a hyperplane. An alpha vector contains one value per state and is the value for the belief state that has a probability of 1 for that state and 0s for all others.   

\bold{Note on the finite grid method:} This method is geared towards infinite-horizon models and will produce strange reward values for finite horizon problems with negative rewards.
  
\bold{Note:} The parser for POMDP files is experimental. Please report problems here: \url{https://github.com/farzad/pomdp/issues}.
}
\value{
The solver returns an object of class POMDP which is a list with 
the model specifications (\code{model}),
the solution (\code{solution}), and 
the solver output (\code{solver_output}).
}

\references{
[1] For further details on how the POMDP solver utilized in this R package works check the following website:
\url{http://www.pomdp.org} 

[2] Cassandra, A. Rocco, Exact and approximate algorithms for partially observable Markov decision processes, (1998). 
\url{https://dl.acm.org/citation.cfm?id=926710}
}
\author{
Hossein Kamalzadeh, Michael Hahsler
}
\examples{
data("Tiger")
Tiger

sol <- solve_POMDP(model = Tiger)
sol

## look at the model
sol$model

## look at solver output
sol$solver_output

## look at the solution
sol$solution

## plot the policy graph of the infinite-horizon POMDP
plot(sol)

## look at the value function (alpha vectors)
alpha <- sol$solution$alpha
alpha

plot_value_function(sol, ylim = c(0,20))

## display available solver options which can be passed on to the solver as parameter.
solve_POMDP_parameter()

## solve a POMDP from http://www.pomdp.org/examples using a grid of size 10
sol <- solve_POMDP("http://www.pomdp.org/examples/cheese.95.POMDP", 
  method = "grid", parameter = list(fg_points = 10))
sol
plot(sol)

## finite-horizon POMDP using the incremental pruning method (without discounting)
sol <- solve_POMDP(model = Tiger, 
  horizon = 3, discount = 1, method = "incprune")
sol

# look at the policy tree
sol$solution$pg
# note: it does not make sense to open the door in epochs 1 or 2 if you only have 3 epochs.

reward(sol) # listen twice and then open the door or listen 3 times
reward(sol, belief = c(1,0)) # listen twice (-2) and then open-left (10)
reward(sol, belief = c(1,0), epoch = 3) # just open the right door (10)
reward(sol, belief = c(.95,.05), epoch = 3) # just open the right door (maybe wrong)

## terminal values 

# Specify 1000 if the tiger is right after horizon epochs
sol <- solve_POMDP(model = Tiger, 
  horizon = 3, discount = 1,  method = "incprune",
  terminal_values = c(0, 1000))
sol

sol$solution$pg
# note: the optimal strategy is never to open the left door, because we get 1000
# as the terminal value if the Tiger is there.

## model changing transition probabilities. 

# The tiger reacts normally for 3 epochs (goes randomly two one
# of the two doors when a door was opened). After 3 epochs he gets 
# scared and when a door is opened then he always goes to the other door.

# 1) create the scared tiger model
Tiger_scared <- Tiger
Tiger_scared$model$transition_prob$"open-left" <- rbind(c(0, 1), c(0, 1))
Tiger_scared$model$transition_prob$"open-right" <- rbind(c(1, 0), c(1, 0))

# 2) Solve in reverse order. Scared tiger without terminal values first.
sol_scared <- solve_POMDP(model = Tiger_scared, 
  horizon = 3, discount = 1,  method = "incprune")
sol_scared
sol_scared$solution$pg

# 3) Solve the regular tiger with the value function of the scared tiger as terminal values
sol <- solve_POMDP(model = Tiger, 
  horizon = 3, discount = 1, method = "incprune",
  terminal_values = sol_scared$solution$alpha[[1]])
sol
sol$solution$pg
# note: it is optimal to mostly listen till the Tiger gets in the scared mood. Only if we are 
# extremely sure in the first epoch, then openig a door is optimal.
}

