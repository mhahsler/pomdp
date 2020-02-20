\name{update_belief}
\alias{update_belief}
\title{Belief Update}
\description{
Update the belief given a taken action and observation.
}
\usage{
update_belief(model, belief = NULL, action = NULL, observation = NULL, 
  episode = 1, digits = 7, drop = TRUE)
}
\arguments{
  \item{model}{ a POMDP model. Defaults to the start belief state specified in the model or "uniform". }
  \item{belief}{ the current belief state. }
  \item{action}{ the taken action. }
  \item{observation}{ the received observation. }
  \item{episode}{ Use transition and observation matrices for the given episode for time-dependent POMDPs (see \code{\link{POMDP}}). }
  \item{digits}{ round decimals. }
  \item{drop}{ logical; drop the result to a vector if only a single belief state is returned. }
}
%\details{
%}
%\value{The resulting belief state.}
%\references{
%}
\author{Michael Hahsler}
\seealso{
\code{\link{POMDP}}
}
\examples{
data(Tiger)

update_belief(c(.5,.5), model = Tiger)
update_belief(c(.5,.5), action = "listen", observation = "tiger-left", model = Tiger)
update_belief(c(.15,.85), action = "listen", observation = "tiger-right", model = Tiger)
}
