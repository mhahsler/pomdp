\name{plot}
\alias{plot.POMDP}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize the POMDP Policy Graph
}
\description{
The function plots the POMDP policy graph provided in an object of classs POMDP.
}
\usage{
\method{plot}{POMDP}(x, y = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP.
}
  \item{y}{
default is NULL
}
  \item{\dots}{
other ploting options passed on to \code{\link[igraph]{plot.igraph}} in \pkg{igraph}.
}
}
%\details{
%}
\value{
the function returns a POMDP policy graph with legends for states and observation arcs
}

\seealso{
\code{\link{solve_POMDP}}
}

\examples{
data("TigerProblem")
sol <- solve_POMDP(model = TigerProblem)
sol

## policy graph
policy_graph(sol)

## visualization
plot(sol)
}

