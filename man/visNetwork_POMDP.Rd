\name{visNetwork_POMDP}
\alias{visNetwork_POMDP}
\title{Visualize the Policy Graph using visNetwork}
\description{
Visualizes the policy graph using \pkg{visNetwork}'s \code{visIgraph} function providing interactive 
inspection and manipulation of the policy.
}
\usage{
visNetwork_POMDP(x, belief = TRUE, cols = NULL, ...)
}
\arguments{
  \item{x}{ A solved POMDP object.}
  \item{belief}{logical; display belief proportions as a pie chart in each node.}
  \item{cols}{colors used for the states.}
  \item{\dots}{Additional arguments are passed on to \code{visIgraph}.}
}
\value{
Returns a htmlwidget of class "visNetwork".
}
\seealso{
\code{\link[visNetwork]{visIgraph}}
}
\examples{
data(TigerProblem)

sol <- solve_POMDP(TigerProblem)

visNetwork_POMDP(sol)

# add smooth edges and a layout
visNetwork_POMDP(sol, smooth = TRUE, layout = "layout_in_circle")
}
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
