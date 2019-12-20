\name{plot_value_function}
\alias{plot_value_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot the Value Function of a POMDP Solution}
\description{
Plots the value function of a POMDP solution as a line plot. The solution is projected on two states (i.e., the belief for the other states is held constant at zero).
}
\usage{
plot_value_function(model, projection = 1:2, ylim = NULL, legend = TRUE)
}
\arguments{
  \item{model}{a solved POMDP.}
  \item{projection}{index or name of two states for the projection. }
  \item{ylim}{the y limits of the plot.}
  \item{legend}{logical; add a legend?}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\author{Michael Hahsler}
\examples{
data("TigerProblem")
tiger_solved <- solve_POMDP(model = TigerProblem)

plot_value_function(tiger_solved, ylim = c(0,20))
}
\keyword{hplot}
