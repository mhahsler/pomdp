\name{policy_graph}
\alias{policy_graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an igraph Policy Graph
}
\description{
Convert the policy graph in a POMDP solution object into a igraph policy graph.
}
\usage{
policy_graph(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A POMDP object.
}
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

policy_graph(tiger_solved)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
