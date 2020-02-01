\name{MDP}
\alias{MDP}

\title{Define an MDP Problem}

\description{Defines all the elements of a MDP problem and formulates them as a POMDP where all states are
observable. This is achieved by defining
one observation per state and identity observation probability matrices.
}

\usage{
MDP(states, actions, transition_prob, reward, 
  discount = 0.9, horizon = Inf, terminal_values = 0, start = "uniform", 
  max = TRUE, name = NA)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{states}{a character vector specifying the names of the states.}
  \item{actions}{a character vector specifying the names of the available actions.}
  \item{transition_prob}{Specifies the transition probabilities between states.} 
  \item{reward}{Specifies the rewards dependent on action, states and observations.} 
  \item{discount}{numeric; discount rate between 0 and 1.}
  \item{horizon}{numeric; Number of epochs. \code{Inf} specifies an infinite horizon.}
  \item{terminal_values}{a vector with the terminal values for each state.}
  \item{start}{Specifies in which state the MDP starts.}
  \item{max}{logical; is this a maximization problem (maximize reward) or a minimization (minimize cost specified in \code{reward})?}
  \item{name}{ a string to identify the MDP problem.}
}
\details{
See \code{\link{POMDP}} for details on specifying the parameters.
}
\value{
The function returns an object of class POMDP which is list with an element called \code{model} containing
a list with the model specification. \code{solve_POMDP} reads the object and adds a list element called
\code{solution}.
}
\seealso{
\code{\link{POMDP}}
}
\author{
Michael Hahsler
}
\examples{
## The Tiger Problem as an MDP with perfect observability

Tiger_MDP <- MDP(
  name = "Tiger Problem",
  discount = 0.75,
  
  states = c("tiger-left" , "tiger-right"),
  actions = c("open-left", "open-right"),
  start = "tiger-left",
  
  transition_prob = list(
    "open-left" =  "uniform", 
    "open-right" = "uniform"),
  

  # the rew helper expects: action, start.state, end.state, observation, value
  reward = rbind(
    R_("open-left",  "tiger-left",  "*", "*", -100),
    R_("open-left",  "tiger-right", "*", "*", 10  ),
    R_("open-right", "tiger-left",  "*", "*", 10  ),
    R_("open-right", "tiger-right", "*", "*", -100)
  )
)
  
Tiger_MDP

# do 5 epochs with no discounting
s <- solve_POMDP(Tiger_MDP, method = "enum", discount = 1, horizon = 5)
s

# policy
plot(s, belief = FALSE)

plot_value_function(s)
}
