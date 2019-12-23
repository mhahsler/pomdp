\name{POMDP}
\alias{POMDP}
\alias{O_}
\alias{R_}
\alias{T_}

\title{Define a POMDP Problem}

\description{Defines all the elements of a POMDP problem including the discount rate, 
the set of states, the set of actions, the set of observations, the transition probabilities, the observation probabilities, and rewards.}

\usage{
POMDP(states, actions, observations, transition_prob, 
      observation_prob, reward, discount = 0.9, start = "uniform", max = TRUE, name = NA)

R_(action, start.state, end.state, observation, value)
O_(action, end.state, observation, probability)
T_(action, start.state, end.state, probability)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{discount}{numeric; discount rate between 0 and 1.}
  \item{states}{a character vector specifying the names of the states.}
  \item{actions}{a character vector specifying the names of the available actions.}
  \item{observations}{a character vector specifying the names of the observations.}
  \item{transition_prob}{Specifies the transition probabilities between states. 
    Options are:
    \itemize{
      \item a data frame with 4 columns, where the columns specify \emph{action}, \emph{start-state}, \emph{end-state} and the \emph{probability} respectively. The first 3 columns could be either character (the name of the action or state) or integer indices.
      \item a named list of \eqn{m} (number of actions) matrices.
  Each matrix is square of size \eqn{n \times n}{n x n}, where \eqn{n} is the number of states. The name of each matrix the action it applies to.
  Instead of a matrix, also the strings \code{"identity"} or \code{"uniform"} can be specified.
    }
  }
  \item{observation_prob}{Specifies the probability that a state produces an observation. 
    Options are:
    \itemize{
      \item a data frame with 4 columns, where the columns specify \emph{action}, \emph{end-state}, \emph{observation} and the \emph{probability}, respectively. The first 3 columns could be either character (the name of the action, state, or observation), integer
  indices, or they can be \code{"*"} to indicate that the observation probability applies to all actions or states. Use \code{rbind()} with helper function \code{O_()} to create this data frame.
      \item a named list of \eqn{m} matrices, where \eqn{m} is the number of actions. Each matrix is of size \eqn{n \times o}{n x o}, where \eqn{n} is the number of states and \eqn{o} is the number of observations. The name of each matrix is the action it applies to.
  Instead of a matrix, also the strings \code{"identity"} or \code{"uniform"} can be specified.
    }
  }
  \item{reward}{Specifies the rewards dependent on action, states and observations. 
    Options are:
    \itemize{
    \item a data frame with 5 columns, where the columns specify \emph{action}, \emph{start.state}, \emph{end.state}, \emph{observation} and the \emph{reward}, respectively. The first 4 columns could be either character (names of the action, states, or observation), integer indices, or they can be \code{"*"} to indicate that the reward applies to all transitions.
     Use \code{rbind()} with helper function \code{R_()} to create this data frame.
    \item a named list of \eqn{m} lists, where \eqn{m} is the number of actions (names should be the actions). Each list contains \eqn{n} named matrices where each matrix is of size \eqn{n \times o}{n x o}, in which \eqn{n} is the number of states and \eqn{o} is the number of observations. Names of these matrices should be the name of states.
  }
  }
  \item{start}{Specifies the initial probabilities for each state (i.e., the initial belief). Some methods
  use this belief to decide which belief states to explore (e.g., the finite grid method). The
  initial beief is also used to find the associated node in the policy graph and to calculate the 
  total expected reward. The default initial belief is a uniform distribution over all states. No 
  initial belief state can be used by setting \code{start = NULL}.
    
  Options to specift start are:
    \itemize{
      \item a probability distribution over the \eqn{n} states. That is, a vector of \eqn{n} probabilities, that add up to \eqn{1}.
      \item the string \code{"uniform"} for a uniform distribution over all states.
      \item an integer in the range \eqn{1} to \eqn{n} to specify a single starting state. or
      \item a string specifying the name of a single starting state.
      \item a vector of strings, specifying a subset of states with a uniform start distribution. If the first element of the vector is \code{"-"}, then the following subset of states is excluded from the set of start states.
    }
  }
  \item{max}{logical; is this a maximization problem (maximize reward) or a minimization (minimize cost specified in \code{reward})?}
  \item{name}{ a string to identify the POMDP problem.}
  \item{action, start.state, end.state, observation, probability, value}{ Values used in the helper functions \code{O_()}, \code{R_()}, and \code{T_()} to create an entry for \code{observation_prob}, \code{reward}, or \code{transistion_prob} above, respectively.}
}
\details{
POMDP problems can be solved using \code{\link{solve_POMDP}}.
Details about the available specifications can be found in [1].
}
\value{
The function returns an object of class POMDP which is list with an element called \code{model} containing
a list with the model specification. \code{solve_POMDP} reads the object and adds a list element called
\code{solution}.
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

Tiger <- POMDP(
  name = "Tiger Problem",
  
  discount = 0.75,
  
  states = c("tiger-left" , "tiger-right"),
  actions = c("listen", "open-left", "open-right"),
  observations = c("tiger-left", "tiger-right"),
  
  start = "uniform",
  
  transition_prob = list(
    "listen" =     "identity", 
    "open-left" =  "uniform", 
    "open-right" = "uniform"),

  observation_prob = list(
    "listen" = rbind(c(0.85, 0.15), 
                     c(0.15, 0.85)),
    "open-left" =  "uniform",
    "open-right" = "uniform"),
  
  # the rew helper expects: action, start.state, end.state, observation, value
  reward = rbind(
    R_("listen",     "*",           "*", "*", -1  ),
    R_("open-left",  "tiger-left",  "*", "*", -100),
    R_("open-left",  "tiger-right", "*", "*", 10  ),
    R_("open-right", "tiger-left",  "*", "*", 10  ),
    R_("open-right", "tiger-right", "*", "*", -100)
  )
)
  
Tiger

Tiger$model
}
