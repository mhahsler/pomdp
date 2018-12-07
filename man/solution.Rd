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
object of class POMDP.
}
}

\value{
returns a list with the solution of a POMDP.
}

\seealso{
\code{\link{solve_POMDP}}
}
\examples{
data("TigerProblem")
result <- solve_POMDP(model = TigerProblem, grid_size = 10, verbose = FALSE)

solution(result)
}
