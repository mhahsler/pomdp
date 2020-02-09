\name{POMDP}
\alias{POMDP}
\alias{O_}
\alias{R_}
\alias{T_}

\title{Define a POMDP Problem}

\description{Defines all the elements of a POMDP problem including the discount rate, 
the set of states, the set of actions, the set of observations, the transition probabilities, the observation probabilities, and rewards.}

\usage{
POMDP(states, actions, observations, transition_prob, observation_prob, reward, 
  discount = 0.9, horizon = Inf, terminal_values = 0, start = "uniform", 
  max = TRUE, name = NA)

R_(action = "*", start.state = "*", end.state = "*", observation = "*", value)
O_(action = "*", end.state = "*", observation = "*", probability)
T_(action = "*", start.state = "*", end.state = "*", probability)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{states}{a character vector specifying the names of the states.}
  \item{actions}{a character vector specifying the names of the available actions.}
  \item{observations}{a character vector specifying the names of the observations.}
  \item{transition_prob}{Specifies action-dependent transition probabilities between states. 
    See Details section.} 
  \item{observation_prob}{Specifies the probability that an action/state combination produces an observation. 
    See Details section.} 
  \item{reward}{Specifies the rewards structure dependent on action, states and observations. 
    See Details section.} 
  \item{discount}{numeric; discount factor between 0 and 1.}
  \item{horizon}{numeric; Number of epochs. \code{Inf} specifies an infinite horizon.}
  \item{terminal_values}{a vector with the terminal values for each state or a matrix specifying the terminal rewards via a terminal value function (e.g., the alpha component produced by solve_POMDP).
  A single 0 specifies that all terminal values are zero.}
  \item{start}{Specifies the initial probabilities for each state (i.e., the initial belief),
  typically as a vector or the string \code{"uniform"} (default). 
  This belief is used to calculate the total expected cumulative reward. It is also used by some
  solvers. See Details section for more information.}
  \item{max}{logical; is this a maximization problem (maximize reward) or a minimization (minimize cost specified in \code{reward})?}
  \item{name}{ a string to identify the POMDP problem.}
  \item{action, start.state, end.state, observation, probability, value}{ Values used in the helper functions \code{O_()}, \code{R_()}, and \code{T_()} to create an entry for \code{observation_prob}, \code{reward}, or \code{transistion_prob} above, respectively. The default value \code{"*"} matches any action/state/observation. }
}
\details{
  POMDP problems can be solved using \code{\link{solve_POMDP}}.
  More details about the available specifications can be found in [1].

  In the following we use the following notation. The POMDP is a 7-duple 
   \eqn{(S,A,T,R, \Omega ,O, \gamma)}. 
   \eqn{S} is the set of states; 
   \eqn{A} is the set of actions; 
   \eqn{T} are the conditional transition probabilities between states;
   \eqn{R} is the reward function;
   \eqn{\Omega} is the set of observations; 
   \eqn{O} are the conditional observation probabilities; and
   \eqn{\gamma} is the discount factor. We will use lower case letters to represent a member of a
   set, e.g., \eqn{s} is a specific state. To refer to the size of a set we will use carnality,
   e.g., the number of actions is \eqn{|A|}.
  
  \bold{Specification of transition probabilities}
 
  Transition probability to transition to state \eqn{s'} from \eqn{s} given action \eqn{a} 
  is \eqn{T(s' | s, a)}. The transition probabilities can be specified
  in the following ways:
  
  \itemize{
    \item A data frame with 4 columns, where the columns specify action \eqn{a}, start-state \eqn{s}, end-state \eqn{s'} and the transition probability \eqn{T(s' | s, a)}, respectively. The first 3 columns can be either character (the name of the action or state) or integer indices. 
    You can use \code{rbind()} with helper function \code{T_()} to create this data frame.
    
    \item A named list of \eqn{|A|} matrices.
    Each matrix is square of size \eqn{|S| \times |S|}{|S| x |S|}. Instead of a matrix, also the strings \code{"identity"} or \code{"uniform"} can be specified.
    }

  \bold{Specification of observation probabilities}
  
  The POMDP specifies the probability for each observation \eqn{o} given an action \eqn{a} and that the system transitioned to a specific state \eqn{s'}, \eqn{O(o | s', a)}. These probabilities can be specified
  in the following ways:
    
    \itemize{
    \item A data frame with 4 columns, where the columns specify the action \eqn{a}, the end-state \eqn{s'}, the observation \eqn{o} and the probability \eqn{O(o | s', a)}, respectively. The first 3 columns could be either character (the name of the action, state, or observation), integer
    indices, or they can be \code{"*"} to indicate that the observation probability applies to all actions or states. 
    You can use \code{rbind()} with helper function \code{O_()} to create this data frame.
    
    \item A named list of \eqn{|A|} matrices. Each matrix is of size \eqn{|S| \times |\Omega|}{|S| x |\Omega|}.
    The name of each matrix is the action it applies to.
    Instead of a matrix, also the string \code{"uniform"} can be specified.
    }
    
  \bold{Specification of the reward function}
  
  The reward function \eqn{R(s, s', o, a)} can be specified
  in the following ways:
  
  \itemize{
  \item a data frame with 5 columns, where the columns specify action \eqn{a}, start.state \eqn{s}, end.state \eqn{s'}, observation \eqn{o} and the associated reward \eqn{R(s, s', a)}, respectively. The first 4 columns could be either character (names of the action, states, or observation), integer indices, or they can be \code{"*"} to indicate that the reward applies to all transitions.
  Use \code{rbind()} with helper function \code{R_()} to create this data frame.
  
  \item a named list of \eqn{|A|} lists. Each list contains \eqn{|S|} named matrices representing the start states \eqn{s}. Each matrix is of size \eqn{|S| \times |\Omega|}{|S| x |\Omega|}, representing the 
  end states \eqn{s'} and observations.
  }
  
  \bold{Start Belief}
  
  This belief is used to calculate the total expected cumulative reward printed with the solved model.
  The function \code{\link{reward}} can be used to calculate rewards for any belief.
  
  Some methods use this belief to decide which belief states to explore (e.g., the finite grid method). 
  The default initial belief is a uniform distribution over all states. No 
  initial belief state can be used by setting \code{start = NULL}.
  
  Options to specify the start belief state are:
  \itemize{
  \item a probability distribution over the states. That is, a vector of \eqn{|S|} probabilities, that add up to \eqn{1}.
  \item the string \code{"uniform"} for a uniform distribution over all states.
  \item an integer in the range \eqn{1} to \eqn{n} to specify the index of a single starting state.
  \item a string specifying the name of a single starting state.
  }

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
  
  # the reward helper expects: action, start.state, end.state, observation, value
  reward = rbind(
    R_("listen",                    v =   -1),
    R_("open-left",  "tiger-left",  v = -100),
    R_("open-left",  "tiger-right", v =   10),
    R_("open-right", "tiger-left",  v =   10),
    R_("open-right", "tiger-right", v = -100)
  )
)
  
Tiger

Tiger$model
}
