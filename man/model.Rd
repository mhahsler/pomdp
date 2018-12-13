\name{model}
\alias{model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return user-defined model components of a POMDP
}
\description{
The function simply returns all the inputs provided by the user as the POMDP model components but in a POMDP-friendly structure
}
\usage{
model(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP.
}
}

\value{
returns a list of all the inputs defined by the user but transformed into POMDP-friendly structure
}

\seealso{
\code{\link{solve_POMDP}}
}
\examples{
data("TigerProblem")
sol <- solve_POMDP(model = TigerProblem)

model(sol)
}
