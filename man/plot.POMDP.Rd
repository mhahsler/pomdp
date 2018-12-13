\name{plot}
\alias{plot.POMDP}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize the POMDP Policy Graph
}
\description{
The function plots the POMDP policy graph provided in an object of class POMDP.
}
\usage{
\method{plot}{POMDP}(x, y = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP.
}
  \item{y}{ ignored.
}
  \item{\dots}{
other plotting options passed on to \code{\link[igraph]{plot.igraph}} in \pkg{igraph}.
}
}
%\details{
%}
%\value{
%}
\seealso{
\code{\link{solve_POMDP}}
}

\examples{
data("TigerProblem")
tiger_solved <- solve_POMDP(model = TigerProblem)
tiger_solved

## policy graph
policy_graph(tiger_solved)

## visualization
plot(tiger_solved)
}
\keyword{hplot}