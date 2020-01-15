\name{transition_matrix}
\alias{transition_matrix}
\alias{observation_matrix}
\alias{reward_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the Transition, Observation or Reward Matrices from a POMDP
}
\description{
Converts the description of transition probabilities and observation probabilities in a POMDP
into a list of matrices, one for each action. Rewards are converted into a list (actions) of lists (start states) of matrices.
}
\usage{
transition_matrix(x)
observation_matrix(x)
reward_matrix(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A POMDP object.}
}
\value{
A list or a list of lists of matrices.
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

# transition matrices for each action in the from states x states
transition_matrix(Tiger)

# observation matrices for each action in the from states x observations
observation_matrix(Tiger)

# reward matrices for each matrix and (start) state in 
# the form (end) state x observation
reward_matrix(Tiger)

# Visualize transition matrix for action 'open-left'
library("igraph")
g <- graph_from_adjacency_matrix(transition_matrix(Tiger)$"open-left", weighted = TRUE)
edge_attr(g, "label") <- edge_attr(g, "weight")

igraph.options("edge.curved" = TRUE)
plot(g, layout = layout_on_grid, main = "Transitions for action 'open=left'")
}
