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
solve_POMDP(model, horizon = Inf, discount = NULL, 
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
  \item{parameter}{ a list with parameters passed on to the pomdp-solve program. }
  \item{verbose}{
logical, if set to \code{TRUE}, the function provides the output of the pomdp solver in the R console.
}
}
\details{
\code{solve_POMDP_parameter()} displays available solver parameter options.

Infinite-horizon POMDPs (\code{horizon = Inf}) converge to a single policy graph. Finite-horizon POMDPs result in a
policy tree of a depth equal to the smaller of the horizon or the number of epochs to convergence. The policty (and the associated alpha values for the value function) are stored in a list by level. The policy for the first epoch is stored as the first element. 

Note on the finite grid method: This method is geared towards infinite-horizon models and will produce strange reward values for finite horizon problems.
  
Note: The parser for POMDP files is experimental. Please report problems here: \url{https://github.com/farzad/pomdp/issues}.
}
\value{
The solver returns an object of class POMDP which is a list with 
the model specifications (\code{model}),
the solution (\code{solution}), and 
the solver output (\code{solver_output}).
The elements can be extracted with the functions  
\code{\link{model}},
\code{\link{solution}}, and
\code{\link{solver_output}}. 
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
data("TigerProblem")
TigerProblem

tiger_solved <- solve_POMDP(model = TigerProblem, parameter = list(fg_points = 10))
tiger_solved

## look at the model
model(tiger_solved)

## look at solver output
solver_output(tiger_solved)

## look at the solution
solution(tiger_solved)

## plot the value function (alpha vectors)
alpha <- solution(tiger_solved)$alpha
alpha

plot(NA, xlim = c(0, 1), ylim = c(0, 20), xlab = "Belief space (for tiger is left)", 
  ylab = "Value function")
for(i in 1:nrow(alpha)) abline(a = alpha[i,2], b = alpha[i,1]- alpha[i,2], col = i, xpd = FALSE)
legend("topright", legend = 
    paste0(1:nrow(alpha),": ", solution(tiger_solved)$pg[,"action"]), col = 1:nrow(alpha), lwd=1)

## plot the policy graph of the infinite-horizon POMDP
plot(tiger_solved)

## display available solver options which can be passed on to the solver as parameter.
solve_POMDP_parameter()

## solve a POMDP from http://www.pomdp.org/examples
sol <- solve_POMDP("http://www.pomdp.org/examples/cheese.95.POMDP")
sol
plot(sol)

## Finite horizon POMDP using the incremental pruning method (without discounting)
tiger_solved <- solve_POMDP(model = TigerProblem, 
  horizon = 3, discount = 1, method = "incprune")
tiger_solved

reward(tiger_solved, belief = "uniform") # listen twice and then open the door ot listen 3 times
reward(tiger_solved, belief = c(1,0)) # open-left = 10 and then listen twice
reward(tiger_solved, belief = c(1,0), epoch = 3) # just open the right door
reward(tiger_solved, belief = c(.95,.05), epoch = 3) # just open the right door (maybe wrong)
}


