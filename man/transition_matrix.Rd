\name{transition_matrix}
\alias{transition_matrix}
\alias{observation_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the Transition or Observation Matrices from a POMDP
}
\description{
Converts the description of transition or observation probabilities in a POMDP in a list of probability matrices, one for each action.
}
\usage{
transition_matrix(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A POMDP object.}
}
\value{
A list with probability matrices.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Michael Hahsler
}
\seealso{
\code{\link{POMDP}}
}
\examples{
data("Tiger")

transition_matrix(Tiger)
observation_matrix(Tiger)

# Visualize transition matrix for action 'open-left'
library("igraph")
g <- graph_from_adjacency_matrix(transition_matrix(Tiger)$"open-left", weighted = TRUE)
edge_attr(g, "label") <- edge_attr(g, "weight")

igraph.options("edge.curved" = TRUE)
plot(g, layout = layout_on_grid, main = "Transitions for action 'open=left'")
}
