\name{transitions}
\alias{transitions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Transition Matrices from a POMDP
}
\description{
Extract the action-dependent transition probability matrices from a POMDP problem specification.  
}
\usage{
transitions(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ an object of class POMDP_model or POMDP.
}
}
\value{
A list with state transition matrices.
}
\author{
Hossein Kamalzadeh, Michael Hahsler
}

\seealso{
\code{\link{POMDP}}
}
\examples{
data("TigerProblem")

trans <- transitions(TigerProblem)
trans

## plot the Markov model for action listening (tiger stays in place) 
library("igraph")

listen <- graph_from_adjacency_matrix(trans$listen, weighted = TRUE)
plot(listen, main = "Action: listen",
  layout = layout_on_grid, vertex.size = 40, edge.arrow.size =.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
