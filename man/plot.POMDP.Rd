\name{plot}
\alias{plot.POMDP}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize a POMDP Policy Graph
}
\description{
The function plots the POMDP policy graph in an object of class POMDP.
It uses \code{plot} in \pkg{igraph} with appropriate plotting options to show information like the belief proportions (if available).}
\usage{
\method{plot}{POMDP}(x, y = NULL, vertex.size = 40, edge.arrow.size =.5, 
  vertex.frame.color = "grey", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP.
}
  \item{y}{ ignored.
}
  \item{vertex.size, edge.arrow.size, vertex.frame.color, \dots}{
plotting options passed on to \code{\link[igraph]{plot.igraph}} in \pkg{igraph}
(see \code{\link[igraph]{plot.common}} for available options).
}
}
%\details{
%}
%\value{
%}
\seealso{
\code{\link{solve_POMDP}},
\code{\link[igraph]{plot.igraph}},
\code{\link[igraph]{igraph_options}},
\code{\link[igraph]{plot.common}}
}

\examples{
data("TigerProblem")
tiger_solved <- solve_POMDP(model = TigerProblem)
tiger_solved

## policy graph
policy_graph(tiger_solved)

## visualization
plot(tiger_solved)

library(igraph)

## use a different graph layout
plot(tiger_solved, layout = layout_with_mds)

## hide edge labels
plot(tiger_solved, edge.label = NA)

## custom larger vertex labels (A, B, ...)
plot(tiger_solved, 
  vertex.label = LETTERS[1:nrow(solution(tiger_solved)$pg)], 
  vertex.label.cex = 2,
  vertex.label.color = "white")

## add a plot title
plot(tiger_solved, main = model(tiger_solved)$name)
}
\keyword{hplot}