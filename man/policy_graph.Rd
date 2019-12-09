\name{policy_graph}
\alias{policy_graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the Policy Graph (as an igraph Object)
}
\description{
Convert the policy graph in a POMDP solution object into an igraph object.
}
\usage{
policy_graph(x, belief = TRUE, cols = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A POMDP object.}
  \item{belief}{logical; add belief proportions as a pie chart in each node of the graph.}
  \item{cols}{colors used for the states in the belief proportions.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
An object of class igraph containing a directed graph.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Hossein Kamalzadeh, Michael Hahsler
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{solve_POMDP}}
}
\examples{
data("TigerProblem")
tiger_solved <- solve_POMDP(model = TigerProblem)
tiger_solved

pg <- policy_graph(tiger_solved)

plot(pg)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
