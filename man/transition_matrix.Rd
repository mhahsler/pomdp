\name{transition_matrix}
\alias{transition_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the Transition Matrices from a POMDP
}
\description{
Converts the description of transitions in a POMDP in a list of transition matrices, one for each
action.
}
\usage{
transition_matrix(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A POMDP object.}
}
\value{
A list with transition matrices.
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

m <- transition_matrix(Tiger)
m 

# Visualize transition matrix for action 'open-left'
library("igraph")
g <- graph_from_adjacency_matrix(m$"open-left", weighted = TRUE)
edge_attr(g, "label") <- edge_attr(g, "weight")

igraph.options("edge.curved" = TRUE)
plot(g, layout = layout_on_grid, main = "Transitions for action 'open=left'")
}
