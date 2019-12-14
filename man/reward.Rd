\name{reward}
\alias{reward}
\title{Calculate the Reward for a POMDP Solution}
\description{
This function calculates the expexted total reward for a POMDP solution given a starting belief state. 
}
\usage{
reward(x, belief = "uniform", epoch = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a POMDP solution (object of class POMDP).}
  \item{belief}{specification of the current belief state (see argument start in \code{\link{POMDP}} for details).}
  \item{epoch}{return reward for this epoch. Default is the first epoch.}
}
\details{
The value is calculated using the value function stored in the POMDP solution.
}
\value{
A list with the components
  \item{total_expected_reward}{the total expected reward given a belief and epoch. }
  \item{belief_state}{the belief state specified in \code{belief}.}
  \item{pg_node}{the policy node that represents the belief state.}
  \item{optimal_action}{the optimal action.}
}
\author{
Michael Hahsler
}
\seealso{
\code{\link{POMDP}},
\code{\link{solve_POMDP}}
}
\examples{
data("TigerProblem")
tiger_solved <- solve_POMDP(model = TigerProblem)

# if no start is specified, a uniform belief is used.
reward(tiger_solved)

# we have additional information that makes us belief that the tiger 
# is more likely to the left.
reward(tiger_solved, belief = c(0.85, 0.15))

# we start with strong evidence that the tiger is to the left.
reward(tiger_solved, belief = "tiger-left")

# Note that in this case, the total discounted expected reward is greater 
# than 10 since the tiger problem resets and another game staring with 
# a uniform belief is played which produces addional reward.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
