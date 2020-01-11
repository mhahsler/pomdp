\name{sample_belief_space}
\alias{sample_belief_space}
\title{Sample from the Belief Space}
\description{Sample randomly (uniform) or regularly spaced points from the projected belief space.}
\usage{
sample_belief_space(model, projection = NULL, n = 1000, random = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ a unsolved or solved POMDP.}
  \item{projection}{ Sample in a projected belief space. All states not included in the projection are held at a belief of 0. \code{NULL} means no projection.  }
  \item{n}{ size of the sample.}
  \item{random}{ logical; sample points randomly or from a regular grid. }
}
\details{
Uses the method described by Luc Deroye to uniformly sample from the belief space.
}
\value{
Returns a matrix. Each row is a sample from the belief space.
}
\references{
Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986. 
}
\author{
Michael Hahsler
}
\examples{
data("Tiger")

sample_belief_space(Tiger, n = 5)
sample_belief_space(Tiger, n = 5, random = FALSE)

# sample and calculate the reward for a solve POMDP
sol <- solve_POMDP(Tiger)
reward(sol, belief= sample_belief_space(sol, n = 5, random = FALSE))
}
