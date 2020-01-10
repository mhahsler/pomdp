\name{plot_belief_space}
\alias{plot_belief_space}
\title{Plot a 2D or 3D Projection of the Belief Space}
\description{
Sample at regular intervals or randomly form the (projected) belief space and plots the optimal action, the node in the policy graph or the reward on a line (2D) or on a ternary plot (3D).
}
\usage{
plot_belief_space(model, projection = NULL, epoch = 1, n = 100, random = FALSE, 
  what = c("action", "pg_node", "reward"), legend = TRUE, pch = 20, col = NULL, ...)
}
\arguments{
  \item{model}{ a solved POMDP.}
  \item{projection}{a vector with state IDs or names to project on. Allowed are projections on two or three states. \code{NULL} uses the first two or three states.}
  \item{epoch}{ display this epoch. }
  \item{n}{ number of samples to use. }
  \item{random}{ logical; sample randomly from the belief space.}
  \item{what}{ what to plot.}
  \item{legend}{ logical; add a legend? If the legend covers the plot then you need to increase the plotting region of the plotting device. }
  \item{pch}{ plotting symbols.}
  \item{col}{ plotting colors.}
  \item{...}{ additional arguments are passed on to \code{plot} for 2D or \code{TerneryPlot} for 3D.}
}
\value{
Returns invisibly the sampled points.
}
\author{
Michael Hahsler
}
\seealso{
\code{\link{sample_belief_space}}
}
\examples{
# two states
data("Tiger")
sol <- solve_POMDP(Tiger)

plot_belief_space(sol)
plot_belief_space(sol, n = 10)
plot_belief_space(sol, n = 10, random = TRUE)
plot_belief_space(sol, what = "pg_node")
plot_belief_space(sol, what = "reward")

# three states
# Note: If the plotting region is too small then the legend might run into the plot 
data("Three_doors")
sol <- solve_POMDP(Three_doors)

plot_belief_space(sol)
plot_belief_space(sol, what = "pg_node")
plot_belief_space(sol, what = "reward")
}
\keyword{hplot}
