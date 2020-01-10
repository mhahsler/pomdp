\name{sample_belief_space}
\alias{sample_belief_space}
\title{Sample from the Belief Space}
\description{Sample randomly (uniform) or regularly spaced points from the belief space and calculate the value and optimal action from the policy.}
\usage{
sample_belief_space(model, projection = NULL, epoch = 1, n = 1000, random = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ a solved POMDP.}
  \item{projection}{ Sample in a projected belief space. All states not included in the projection are held at a belief of 0. \code{NULL} means no projection.  }
  \item{epoch}{ evaluate the value/action for what epoch? Ignorred for infinite-horizon solutions.}
  \item{n}{ size of the sample.}
  \item{random}{ logical; sample points randomly or from a regular grid. }
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
data("Tiger")

sol <- solve_POMDP(Tiger)
sample_belief_space(sol, n = 5)
}
