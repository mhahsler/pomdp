\name{plot}
\alias{plot.POMDP}
\alias{plot}

\title{Visualize a POMDP Policy Graph}

\description{The function plots the POMDP policy graph in an object of class POMDP. It uses \code{plot} in \pkg{igraph} with appropriate plotting options.}

\usage{
\method{plot}{POMDP}(x, y = NULL, belief = TRUE, legend = TRUE, 
  vertex.size = 40, edge.arrow.size =.5, cols = NULL,...)
}
\arguments{
  \item{x}{object of class POMDP.}
  \item{y}{ignored.}
  \item{belief}{logical; display belief proportions as a pie chart in each node.}
  \item{legend}{logical; display a legend for colors used belief proportions?}
  \item{cols}{colors used for the states.}
  \item{vertex.size, edge.arrow.size, \dots}{plotting options passed on to \code{\link[igraph]{plot.igraph}} in \pkg{igraph} (see \code{\link[igraph]{plot.common}} for available options).}
}

\details{
The policy graph nodes represent segments in the value function. Each segment represents one or more believe states. The pie chart in each node (if available) represent the average belief proportions of the belief states belonging to the node/segment.
}
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

## use a different graph layout (circle and manual)
plot(tiger_solved, layout = layout.circle)
plot(tiger_solved, layout = rbind(c(1,1), c(1,-1), c(0,0), c(-1,-1), c(-1,1)))

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