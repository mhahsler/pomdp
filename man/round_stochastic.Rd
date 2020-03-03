\name{round_stochastic}
\alias{round_stochastic}
\title{
Round a stochastic vector or a row-stochastic matrix
}
\description{
Rounds a vector such that the sum of 1 is preserved. Rounds a matrix such that the 
rows still sum up to 1.
}
\usage{
round_stochastic(x, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a stochastic vector or a row-stochastic matrix.}
  \item{digits}{number of digits for rounding.}
}
\details{
Rounds and adjusts one entry such that the rounding error is the smalles.
}
\value{
The rounded vector or matrix.
}
\seealso{
\link{round}
}
\examples{
x <- c(0.25, 0.25, 0.5)
round_stochastic(x, 2)
round_stochastic(x, 1)
round_stochastic(x, 0)
}
