\name{plot_value_function}
\alias{plot_value_function}
\title{Plot the Value Function of a POMDP Solution}
\description{
Plots the value function of a POMDP solution as a line plot. The solution is projected on two states (i.e., the belief for the other states is held constant at zero).
}
\usage{
plot_value_function(model, projection = 1:2, epoch = 1, ylim = NULL, legend = TRUE, 
  col = NULL, lwd = 1, lty = 1, ...)
}
\arguments{
  \item{model}{a solved POMDP.}
  \item{projection}{index or name of two states for the projection. }
  \item{epoch}{the value function of what epoch should be plotted? Ignorred for infinite-horizon solutions. }
  \item{ylim}{the y limits of the plot.}
  \item{legend}{logical; add a legend?}
  \item{col}{potting colors.}
  \item{lwd}{line width.}
  \item{lty}{line type.}
  \item{...}{additional arguments are passed on to \code{line}.}
}
\author{Michael Hahsler}
\examples{
data("Tiger")
sol <- solve_POMDP(model = Tiger)
sol

plot_value_function(sol, ylim = c(0,20))

## finite-horizon
sol <- solve_POMDP(model = Tiger, horizon = 3, discount = 1, 
  method = "enum")
sol

plot_value_function(sol, epoch =1, ylim = c(-5, 25))
plot_value_function(sol, epoch =2, ylim = c(-5, 25))
plot_value_function(sol, epoch =3, ylim = c(-5, 25))
}
\keyword{hplot}
