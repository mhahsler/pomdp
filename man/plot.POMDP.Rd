\name{plot}
\alias{plot.POMDP}
\alias{plot_policy_graph}
\alias{plot}

\title{Visualize a POMDP Policy Graph}

\description{The function plots the POMDP policy graph in an object of class POMDP. It uses \code{plot} in \pkg{igraph} with appropriate plotting options.}

\usage{
plot_policy_graph(x, belief = TRUE, legend = TRUE, cols = NULL, 
  engine = c("igraph", "visNetwork"), ...)

\method{plot}{POMDP}(x, y = NULL, ...)
}
\arguments{
  \item{x}{object of class POMDP containing a solved POMDP problem.}
  \item{y}{ignored.}
  \item{belief}{logical; display belief proportions as a pie chart in each node.}
  \item{legend}{logical; display a legend for colors used belief proportions?}
  \item{cols}{colors used for the states.}
  \item{engine}{The plotting engine to be used.}
  \item{\dots}{plotting options passed on to the plotting engine (see Details section).} 
}

\details{
The method \code{plot} for objects of class POMDP calls for convenience \code{plot_policy_graph}.

The policy graph nodes represent segments in the value function. Each segment represents one or more believe states. The pie chart in each node (if available) represent the average belief proportions of the belief states belonging to the node/segment.

The built in plotting engines are \pkg{igraph} and \pkg{visNetwork}. The additional arguments specified in \code{...} are passed on to the engine plotting function. 
For \pkg{igraph} this is  \code{\link[igraph]{plot.igraph}} (see \code{\link[igraph]{plot.common}} for available options).
For \pkg{visNetwork} this is \code{\link[visNetwork]{visIgraph}}. 

Other plotting libraries can be used by creating a policy graph (as an igraph object) using \code{\link{policy_graph}} and converting it into a suitable representation for that library.  
}
%\value{
%}
\seealso{
\code{\link{solve_POMDP}},
\code{\link{policy_graph}}.

From: \pkg{igraph}
\code{\link[igraph]{plot.igraph}},
\code{\link[igraph]{igraph_options}},
\code{\link[igraph]{plot.common}}.

From \pkg{visNetwork}:
\code{\link[visNetwork]{visIgraph}}.
}

\examples{
data("TigerProblem")
tiger_solved <- solve_POMDP(model = TigerProblem)
tiger_solved

## policy graph
policy_graph(tiger_solved)

## visualization
plot(tiger_solved)

## use a different graph layout (circle and manual; needs igraph)
library("igraph")
plot(tiger_solved, layout = layout.circle)
plot(tiger_solved, layout = rbind(c(1,1), c(1,-1), c(0,0), c(-1,-1), c(-1,1)))

## hide labels and legend
plot(tiger_solved, edge.label = NA, vertex.label = NA, legend = FALSE)

## add a plot title
plot(tiger_solved, main = tiger_solved$model$name)

## custom larger vertex labels (A, B, ...)
plot(tiger_solved, 
  vertex.label = LETTERS[1:nrow(tiger_solved$solution$pg)], 
  vertex.label.cex = 2,
  vertex.label.color = "white")

## plotting using the graph object 
## (e.g., using the graph in the layout and to change the edge curvature)
pg <- policy_graph(tiger_solved)
plot(pg, 
  layout = layout_as_tree(pg, root = 3, mode = "out"),
  edge.curved = curve_multiple(pg, .2))

## changes labels
plot(pg, 
  edge.label = abbreviate(E(pg)$label),
  vertex.label = tiger_solved$solution$pg$action,
  vertex.size = 10)

## plot interactive graphs using the visNetwork library
plot(tiger_solved, engine = "visNetwork")

## add smooth edges and a layout (note, engine can be abbreviated)
plot(tiger_solved, engine = "vis", smooth = TRUE, layout = "layout_in_circle")
}
\keyword{hplot}
