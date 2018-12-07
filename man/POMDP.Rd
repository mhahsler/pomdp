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
      observation_prob, reward, values = "reward")
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
  \item{start}{Specifies the initial probabilities of being in each state. Options are:
  \itemize{
  \item a vector of \emph{n} probabilities, that add up to \code{1}, where \emph{n} is the number of states, or
  \item the string "uniform" for a uniform distribution over all states, or
  \item an integer in the range 1 to \emph{n} to specify a single starting state), or
  \item a string specifying the name of a single starting state, or
  \item a vector of strings, specifying a subset of states with a uniform start distribution. If the first element of the vector is \code{"-"}, then the following subset of states is excluded from the set of start states.
}
}
  \item{transition_prob}{Specifies the transition probabilities between states. Options are:
  \itemize{
  \item a dataframe with 4 columns, where the columns specify \emph{action}, \emph{start-state}, \emph{end-state} and the \emph{probability} respectively. The first 3 columns could be either character (the name of the action or state) or integers starting from 0 (the number associated with the action or state e.g., 0 as the first action or 2 as the third state), or
  \item a named list of \emph{m} matrices where \emph{m} is the number of actions, each matrix is square of size \emph{n x n} where \emph{n} is the number of states. The name of each matrix need to be the action it applies to.
  Instead of a matrix, also the strings "identity" or "uniform" can be specified.
}
}
  \item{observation_prob}{Specifies the observation probabilities. Options are:
  \itemize{
  \item a dataframe with 4 columns, where the columns specify \emph{action}, \emph{end-state}, \emph{observation} and the \emph{probability}, respectively. The first 3 columns could be either character (the name of the action, state, or observation) or an integer starting from 0 (the number associated with the action, state, or observation) or they can be "*" to indicate that the observation probability applies to all
  actions or states.
  \item a named list of \emph{m} matrices where \emph{m} is the number of actions. Each matrix is of size \emph{\eqn{n x o}} where \emph{n} is the number of states and \emph{o} is the number of observations. The name of each matrix need to be the action it applies to.
  Instead of a matrix, also the strings "identity" or "uniform" can be specified.
}
}
  \item{reward}{Specifies rewards. Options are:
  \itemize{
  \item a dataframe with 5 columns, where the columns specify \emph{action}, \emph{start-state}, \emph{end-state}, \emph{observation} and the \emph{reward}, respectively. The first 4 columns could be either character (the name of the action, state, or observation) or numeric with an integer starting from 0 (the number associated with the action, state, observation) or they can be "*" to indicate that the reward applies to all transitions with possible values.
  \item a matrix of size \emph{\eqn{n x o}} where \emph{n} is the number of states and \emph{o} is the number of observations.
}
}
  \item{values}{
a string indicating if rewards are specified as a "reward" or a "cost". The default is reward.
}
}
\value{
The function returns a list with the model specification.
}
\details{
Details about the available specifications can be found in [1].
}
\references{
[1] For further details on how the POMDP solver utilized in this R package works check the following website:
\url{http://www.pomdp.org} 
}
\author{
Hossein Kamalzadeh, Michael Hahsler
}
\examples{
## The Tiger Problem

TigerProblem <- POMDP(
  discount = 0.75,
  
  states = c("tiger-left" , "tiger-right"),
  start = "uniform",
  
  transition_prob = list(
    "listen" = "identity", 
    "open-left" = "uniform", 
    "open-right" = "uniform"),

  actions = c("listen", "open-left", "open-right"),

  observations = c("tiger-left", "tiger-right"),
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
  values = "reward"
  )
  
TigerProblem
}

