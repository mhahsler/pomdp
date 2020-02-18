\name{simulate_POMDP}
\alias{simulate_POMDP}
\title{Simulate Trajectories in a POMDP}
\description{
Simulate several trajectories through a POMDP. The start state for each trajectory is randomly chosen using the specified belief. For solved POMDPs the optimal actions will be chosen, for unsolved POMDPs random actions will be used.}
\usage{
simulate_POMDP(model, n= 100, belief = NULL, horizon = NULL, visited_beliefs = FALSE, 
  random_actions = NULL, digits = 7, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ a POMDP model. }
  \item{n}{ number of trajectories. }
  \item{belief}{ probability distribution over the states for choosing the starting states for the trajectories. }
  \item{horizon}{ number of epochs for the simulation. If \code{NULL} then the horizon for the model is used. }
  \item{visited_beliefs}{ logical; Should all belief points visited on the trajectories be returned? If \code{FALSE} then only the belief at the final epoch is returned.}
  \item{random_actions}{ logical; should random actions be used. If \code{NULL} then a policy is used when available (i.e., for a solved POMDP) and randomized actions, otherwise. }
  \item{digits}{ round belief points. }
  \item{verbose}{ report used parameters. }
}
%\details{
%}
\value{
A matrix with belief points as rows. Attributes containing action counts, and rewards may be available.
}
\author{Michael Hahsler}

\seealso{
\code{\link{POMDP}}
}
\examples{
data(Tiger)

# solve the POMDP for 5 epochs and no discounting
sol <- solve_POMDP(Tiger, horizon = 5, discount = 1, method = "enum")
sol
policy(sol)

## Example 1: simulate 10 trajectories, only the final belief state is returned
sim <- simulate_POMDP(sol, n = 100, verbose = TRUE)
head(sim)

# plot the final belief state, look at the average reward and how often different actions were used.
plot_belief_space(sol, sample = sim)

# additional data is available as attributes
names(attributes(sim))
attr(sim, "avg_reward")
colMeans(attr(sim, "action"))


## Example 2: look at all belief states in the trajectory starting with an initial start belief.
sim <- simulate_POMDP(sol, n = 100, belief = c(.5, .5), visited_beliefs = TRUE)
plot_belief_space(sol, sample = sim)

# add density
lines(density(sim[,1], bw = .05))
axis(2); title(ylab = "Density")


## Example 3: simulate trajectories for an unsolved POMDP (actions are taken randomly)
sim <- simulate_POMDP(Tiger, n = 100, horizon = 5, visited_beliefs = TRUE)
plot_belief_space(sol, sample = sim)
}
