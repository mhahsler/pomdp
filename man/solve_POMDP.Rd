\name{solve_POMDP}
\alias{solve_POMDP}
\alias{solve_POMDP_parameter}
\title{Solve a POMDP Problem}
\description{
This function utilizes the 'pomdp-solve' program (written in C) to solve problems that are formulated as partially observable Markov decision processes (POMDPs) [1]. The user can specify what method or algorithm should be used by the solver to solve the given problem [2].
Given there is an optimal solution, the function provides the optimal solution including the optimal policy. 
}
\usage{
solve_POMDP(model, horizon = NULL, method = "grid", parameter= NULL, verbose = FALSE)
solve_POMDP_parameter()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ a POMDP problem created with \code{\link{POMDP}}.}
  \item{method}{ string; one of the following solution methods: "grid", "enum", "twopass", "witness", or "incprune". Details can be found in [1].}
  \item{horizon}{ an integer with the number of iterations for finite horizon problems. If set to \code{NULL}, the algorithm runs iterations till it converges to the infinite horizon solution.}
  \item{parameter}{ a list with parameters passed on to pomdp-solve. }
  \item{verbose}{
logical, if set to true, the function provides the output of the pomdp solver in R console.
}
}
\details{
\code{solve_POMDP_parameter()} displays available solver parameter options.
}
\value{
The solver returns a list with the model specifications (\code{model}),
the solution (\code{solution}), and the solver output (\code{solver_output}).
The elements can be extracted with the functions  
\code{\link{model}},
\code{\link{solution}}, and
\code{\link{solver_output}}. Th returned object is of class POMDP.
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
\note{
\strong{'pomdp-solve'} program uses the basic dynamic programming approach, solving one stage at a time working backward in time. It does finite horizon problems with or without discounting. It will stop solving if the answer is within a tolerable range of the infinite horizon answer, and there are a couple of different stopping conditions (requires a discount factor less than 1.0). Alternatively, you can solve a finite horizon problem for some fixed horizon length.
}

\examples{
data("TigerProblem")

tiger_solved <- solve_POMDP(model = TigerProblem, parameter = list(fg_points = 10))
tiger_solved

## look at the model
model(tiger_solved)

## look at the solution
solution(tiger_solved)

## plot the policy graph
plot(tiger_solved)

## display solver options
solve_POMDP_parameter()
}


