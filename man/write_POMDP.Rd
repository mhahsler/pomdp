\name{write_POMDP}
\alias{write_POMDP}
\title{
Write a POMDP Model to a File in POMDP Format
}
\description{
Writes a POMDP file suitable for pomdp-solve.
}
\usage{
write_POMDP(model, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ an object of class POMDP_model.
}
  \item{file}{ a filename.
}
}
%\details{
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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
