\name{solution}
\alias{solution}
\title{
Extract the Solution of a POMDP
}
\description{
The function extracts the solution of a POMDP as an object of class POMDP_solution which is a list containing, e.g., the policy graph (pg) and the hyper-plane coefficients (alpha).
}
\usage{
solution(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP returned by \code{\link{solve_POMDP}}.
}
}

\value{
returns an object is of class POMDP_solution, i.e., a list of all solution elements.
}

\seealso{
\code{\link{solve_POMDP}}
}
\examples{
data("TigerProblem")
tiger_solved <- solve_POMDP(model = TigerProblem)
tiger_solved

solution(tiger_solved)
}
