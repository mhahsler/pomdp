\name{POMDP}
\alias{POMDP}
\title{
Define a POMDP Problem
}
\description{
Defines all the elements of a POMDP problem including discount rate, states, actions, observations, transition probabilities, observation probabilities, and rewards.
}
\usage{
POMDP(discount, states, actions, observations, start, transition_prob, 
      observation_prob, reward, values = "reward", name = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{discount}{
numeric, discount rate where 1 is 100\%.
}
  \item{states}{
character, a vector of strings specifying the names of the states.
}
  \item{actions}{
character, a vector of strings specifying the names of the available actions.
}
  \item{observations}{
character, a vector of strings specifying the names of the observations.
}
  \item{start}{Specifies the initial probabilities for each state. Options are:
  \itemize{
  \item a probability distribution over the \eqn{n} states. That is, a vector of \eqn{n} probabilities, that add up to \eqn{1}.
  \item the string \code{"uniform"} for a uniform distribution over all states.
  \item an integer in the range \eqn{1} to \eqn{n} to specify a single starting state. or
  \item a string specifying the name of a single starting state.
  \item a vector of strings, specifying a subset of states with a uniform start distribution. If the first element of the vector is \code{"-"}, then the following subset of states is excluded from the set of start states.
}
}
  \item{transition_prob}{Specifies the transition probabilities between states. Options are:
  \itemize{
  \item a data frame with 4 columns, where the columns specify \emph{action}, \emph{start-state}, \emph{end-state} and the \emph{probability} respectively. The first 3 columns could be either character (the name of the action or state) or integer indices.
  \item a named list of \eqn{m} (number of actions) matrices.
  Each matrix is square of size \eqn{n \times n}{n x n}, where \eqn{n} is the number of states. The name of each matrix the action it applies to.
  Instead of a matrix, also the strings \code{"identity"} or \code{"uniform"} can be specified.
}
}
  \item{observation_prob}{Specifies the observation probabilities. Options are:
  \itemize{
  \item a data frame with 4 columns, where the columns specify \emph{action}, \emph{end-state}, \emph{observation} and the \emph{probability}, respectively. The first 3 columns could be either character (the name of the action, state, or observation), integer
  indices, or they can be \code{"*"} to indicate that the observation probability applies to all actions or states.
  \item a named list of \eqn{m} matrices, where \eqn{m} is the number of actions. Each matrix is of size \eqn{x \times o}{n x o}, where \eqn{n} is the number of states and \eqn{o} is the number of observations. The name of each matrix is the action it applies to.
  Instead of a matrix, also the strings \code{"identity"} or \code{"uniform"} can be specified.
}
}
  \item{reward}{Specifies the rewards. Options are:
  \itemize{
  \item a data frame with 5 columns, where the columns specify \emph{action}, \emph{start-state}, \emph{end-state}, \emph{observation} and the \emph{reward}, respectively. The first 4 columns could be either character (names of the action, states, or observation), integer indices, or they can be \code{"*"} to indicate that the reward applies to all transitions.
  \item a matrix of size \eqn{n \times o}{n x o} where \eqn{n} is the number of states and \eqn{o} is the number of observations.
}
}
  \item{values}{
a string indicating if the specified rewards represent a \code{"reward"} or a \code{"cost"}. The default is reward.
}
  \item{name}{ a string to identify the POMDP problem.}

}
\details{
POMDP problems can be solved using \code{\link{solve_POMDP}}.
Details about the available specifications can be found in [1].
}
\value{
The function returns an object of class POMDP_model which is list with the model specification.
}
\references{
[1] For further details on how the POMDP solver utilized in this R package works check the following website:
\url{http://www.pomdp.org} 
}
\seealso{
\code{\link{solve_POMDP}}
}
\author{
Hossein Kamalzadeh, Michael Hahsler
}
\examples{
## The Tiger Problem

TigerProblem <- POMDP(
  name = "Tiger Problem",
  
  discount = 0.75,
  
  states = c("tiger-left" , "tiger-right"),
  actions = c("listen", "open-left", "open-right"),
  observations = c("tiger-left", "tiger-right"),
  
  start = "uniform",
  
  transition_prob = list(
    "listen" = "identity", 
    "open-left" = "uniform", 
    "open-right" = "uniform"),

  observation_prob = list(
    "listen" = matrix(c(0.85, 0.15, 0.15, 0.85), nrow = 2, byrow = TRUE), 
    "open-left" = "uniform",
    "open-right" = "uniform"),
    
  reward = data.frame(
    "action" = c("listen", "open-left", "open-left", "open-right", "open-right"),
    "start-state" = c("*", "tiger-left", "tiger-right", "tiger-left", "tiger-right"),
    "end-state" = c("*", "*", "*", "*", "*"),
    "observation" = c("*", "*", "*", "*", "*"),
    "reward" = c(-1, -100, 10, 10, -100)),
  )
  
TigerProblem
}