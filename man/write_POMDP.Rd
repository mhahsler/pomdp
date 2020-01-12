\name{write_POMDP}
\alias{write_POMDP}
\alias{read_POMDP}
\title{
Read and write a POMDP Model to a File in POMDP Format
}
\description{
Reads and write a POMDP file suitable for the pomdp-solve program.
}
\usage{
write_POMDP(model, file)
read_POMDP(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ an object of class POMDP_model.
}
  \item{file}{ a file name.
}
}
%\details{
%}
\value{
\code{read_POMDP} returns a POMDP object.
}
\references{
POMDP solver website: \url{http://www.pomdp.org} 
}
\author{
Hossein Kamalzadeh, Michael Hahsler
}

\seealso{
\code{\link{POMDP}}
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
