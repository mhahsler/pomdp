\name{policy}
\alias{policy}
\title{
Extract the Policy from a POMDP/MDP
}
\description{
Extracts the policy from a solves POMDP/MDP.
}
\usage{
policy(x)
}
\arguments{
  \item{x}{ A solved POMDP object.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with the policy for each epoch.
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
Michael Hahsler
}
%\note{
%%  ~~further notes~~
%}

%\seealso{
%}
\examples{
data("Tiger")
sol <- solve_POMDP(model = Tiger)

policy(sol)
sol

pg <- policy_graph(sol)

plot(pg)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
