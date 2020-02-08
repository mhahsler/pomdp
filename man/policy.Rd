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
\details{
A list (one entry per epoch) with the optimal policy. The policy is a data frame consisting of three parts.

Part 1: The value function with one column per state. (For MDPs this is just one column with the state).

Part 2: One column with the optimal action.

Part 3: One column per observation with the index of the row representing the policy node in the next epoch.
  
}
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
sol

# policy with value function, optimal action and transitions for observations.
policy(sol)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
