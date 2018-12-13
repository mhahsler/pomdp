\name{solution}
\alias{solution}
\title{
Return the Solution of a POMDP
}
\description{
The function extracts the solution of a POMDP as a list.
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
returns a list with the solution of a POMDP. The returned object is of class POMDP_solution.
}

\seealso{
\code{\link{solve_POMDP}}
}
\examples{
data("TigerProblem")
result <- solve_POMDP(model = TigerProblem)

solution(result)
}
