\name{sample_belief_space}
\alias{sample_belief_space}
\title{Sample from the Belief Space}
\description{Sample uniformly points from the belief space and calculte the value and optimal action from the policy.}
\usage{
sample_belief_space(model, n = 1000, projection = NULL, epoch = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ a solved POMDP.}
  \item{n}{ size of the sample.}
  \item{projection}{ Sample in a projected belief space. All states not included in the projection are held at a belief of 0. \code{NULL} means no projection.  }
  \item{epoch}{ evaluate the value/action for what epoch? Ignorred for infinite-horizon solutions.}
}
\details{
Uses the method described by Luc Deroye to uniformly sample from the belief space.
}
\value{
Returns a list with the components
\item{belief}{ the sampled points from the belief space. }
\item{optimal}{ a data frame with the expected reward, the node in the policy graph (segment of the value function), and the optimal action. }
}
\references{
Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986. 
}
\author{
Michael Hahsler
}
\examples{
data("TigerProblem")

sol <- solve_POMDP(TigerProblem)
sample_belief_space(sol, n = 5)
}
