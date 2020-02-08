\name{sample_belief_space}
\alias{sample_belief_space}
\title{Sample from the Belief Space}
\description{Sample randomly (uniform) or regularly spaced points from the projected belief space.}
\usage{
sample_belief_space(model, projection = NULL, n = 1000, method = "random")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ a unsolved or solved POMDP.}
  \item{projection}{ Sample in a projected belief space. All states not included in the projection are held at a belief of 0. \code{NULL} means no projection.  }
  \item{n}{ size of the sample.}
  \item{method}{ character string specifying the sampling strategy. Available are \code{"random"}, \code{"regular"},
    and \code{"vertices"}.}
}
\details{
Method random samples uniformly sample from the projected belief space using the method described by Luc Deroye. Method regular samples points using a regularly spaced grid. This method is only available for projections on 2 or 3 states. 
Method vertices only samples from the vertices of the belief space.  
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
sample_belief_space(Tiger, n = 5, method = "regular")

# sample and calculate the reward for a solve POMDP
sol <- solve_POMDP(Tiger)
reward(sol, belief = sample_belief_space(sol, n = 5, method = "regular"))
}
