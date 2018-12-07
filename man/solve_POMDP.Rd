\name{solve_POMDP}
\alias{solve_POMDP}
\title{Solve a POMDP Problem}
\description{
This function utilizes the 'pomdp-solve' program (written by in C) to solve problems that are formulated as partially observable Markov decision processes (POMDPs) [1]. The function currently only interfaces the Finite Grid method [2].
Given there is an optimal solution, the function provides the optimal solution including the optimal policy. 
}
\usage{
solve_POMDP(model, grid_size, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ a POMDP problem created with \code{\link{POMDP}}.}
  \item{grid_size}{an integer that specifies the size of the initial grid of the finite grid method.
}
  \item{verbose}{
logical, if set to true, the function provides the output of the pomdp solver in R console.
}
}
\value{
The solver returns a list with the model specifications (\code{model}),
the solution (\code{solution}), and the solver ourtput (\code{solver_output}).
The elements can be extracted with the functions  
\code{\link{model}},
\code{\link{solution}}, and
\code{\link{solver.output}}.
}

\references{
[1] For further details on how the POMDP solver utilized in this R package works check the following website:
\url{http://www.pomdp.org} 

[2] Cassandra, A. Rocco, Exact and approximate algorithms for partially observable markov decision processes, (1998). 
\url{https://dl.acm.org/citation.cfm?id=926710}
}
\author{
Hossein Kamalzadeh, Michael Hahsler
}
\note{
\strong{'pomdp-solve'} program uses the basic dynamic programming approach, solving one stage at a time working backwards in time. It does finite horizon problems with or without discounting. It will stop solving if the answer is within a tolerable range of the infinite horizon answer, and there are a couple of different stopping conditions (requires a discount factor less than 1.0). Alternatively you can solve a finite horizon problem for some fixed horizon length.
}

\examples{
data("TigerProblem")

sol <- solve_POMDP(model = TigerProblem, grid_size = 10)

model(sol)
solution(sol)
}

