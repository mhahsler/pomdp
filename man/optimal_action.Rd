\name{optimal_action}
\alias{optimal_action}
\title{Optimal action for a belief}
\description{
Detemines the optimal action for a policy (solved POMDP) for a given belief at a given epoch.
}
\usage{
optimal_action(model, belief, epoch = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{a solved POMDP model.}
  \item{belief}{The belief (probability distribution over the states) as a vector or a matrix with multiple belief states as rows. }
  \item{epoch}{what epoch of the policy should be used. }
}
%\details{
%}
\value{
The name of the optimal action.
}
\author{
Michael Hahsler
}
\seealso{
\code{\link{POMDP}}
}
\examples{
data("Tiger")
Tiger

sol <- solve_POMDP(model = Tiger)

# these are the states
sol$model$states

# belief that tiger is to the left
optimal_action(sol, c(1, 0))
optimal_action(sol, "tiger-left")

# belief that tiger is to the right
optimal_action(sol, c(0, 1))
optimal_action(sol, "tiger-right")

# belief is 50/50
optimal_action(sol, c(.5, .5))
optimal_action(sol, "uniform")

# the POMDP is converged, so all epoch give the same result.
optimal_action(sol, "tiger-right", epoch = 10)
}
