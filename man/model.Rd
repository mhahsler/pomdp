\name{model}
\alias{model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the User-defined Model Components from a Solved POMDP
}
\description{
The function returns all the POMDP model components of a solved POMDP.
}
\usage{
model(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP returned by \code{\link{solve_POMDP}}.
}
}

\value{
an object of class "POMDP_model", i.e., a list of all model components.
}

\seealso{
\code{\link{solve_POMDP}}
}
\examples{
data("TigerProblem")
tiger_solved <- solve_POMDP(model = TigerProblem)
tiger_solved

model(tiger_solved)
}
