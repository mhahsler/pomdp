\name{pomdp}
\alias{pomdp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Solve a POMDP
}
\description{
This function utilizes the 'pomdp-solve' program (written in C) to solve problems that are formulated as partially observable Markov decision processes, a.k.a. POMDPs [1].  The solver actually have the option to use various POMDP solution algorithms to solve problems but the pomdp function in this R package uses only the Finite Grid method [2] to do so. The function provides user with various ways of defining the pomdp components such as states, actions, transitions and rewards and then transforms all the inputs into pomdp-friendly structures and feeds them to 'pomdp-solver'. Given there is an optimal solution, the function provides the optimal solution including the optimal policy. 
}
\usage{
pomdp(discount, states, actions, observations, start, transition_prob, 
      observation_prob, reward, values = "reward", grid_size, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{discount}{
numeric, a real number in \code{[0,1]}
}
  \item{states}{
character, a vector of strings specifying the names of the states
}
  \item{actions}{
character, a vector of strings specifying the names of the actions
}
  \item{observations}{
character, a vector of strings specifying the names of the observations
}
  \item{start}{indicates the initial probabilities of being in each state. options are:
  \itemize{
  \item a vector of \emph{n} numbers each in \code{[0,1]}, that add up to \code{1}, where \emph{n} is the number of states, or
  \item character, a string with the word "uniform" (a uniform distribution over all states), or
  \item numeric, an integer in 1 to \emph{n} (to specify a single starting state), or
  \item character, a string specifying the name of a single state (to specify a single starting state), or
  \item character, a vector of strings, specifying a subset of states (a uniform distribution over a subset of states) (the first element of the vector should be \code{"-"} if the following subset of states is supposed to be excluded)
}
}
  \item{transition_prob}{contains the transition probabilities between states. options are:
  \itemize{
  \item a dataframe with 4 columns, where the columns specify \emph{action}, \emph{start-state}, \emph{end-state} and the \emph{probability} respectively. The frist 3 columns could be either character (the name of the action or state) or numeric with an integer starting from 0 (the number associated with the action or state e.g., 0 as the first action or 2 as the third state)
  \item a list of \emph{m} matrices where \emph{m} is the number of actions, each matrix is square of size \emph{n} where \emph{n} is the number of states (each matrix should have a name in the list and the name should be one of the actions). Also each matrix can be defined using "identity" or "uniform"
}
}
  \item{observation_prob}{options are:
  \itemize{
  \item a dataframe with 4 columns, where the columns specify \emph{action}, \emph{end-state}, \emph{observation} and the \emph{probability} respectively. The frist 3 columns could be either character (the name of the action, state, or observation) or numeric with an integer starting from 0 (the number associated with the action, state or observation e.g., 0 as the first action or 2 as the third state) or they can be "*" to indicate the independency
  \item a list of \emph{m} matrices where \emph{m} is the number of actions, each matrix is of size \emph{\eqn{nxo}} where \emph{n} is the number of states and \emph{o} is the number of observations (each matrix should have a name in the list and the name should be one of the actions). Also each matrix can be defined using "uniform"
}
}
  \item{reward}{options are:
  \itemize{
  \item a dataframe with 5 columns, where the columns specify \emph{action}, \emph{start-state}, \emph{end-state}, \emph{observation} and the \emph{reward} respectively. The frist 4 columns could be either character (the name of the action, state, or observation) or numeric with an integer starting from 0 (the number associated with the action, state, observation e.g., 0 as the first action or 2 as the third state) or they can be "*" to indicate the independency
  \item numeric, a matrix of size \emph{\eqn{nxo}} where \emph{n} is the number of states and \emph{o} is the number of observations.
}
}
  \item{values}{
character, a string with either "reward" or "cost". The default is reward
}
  \item{grid_size}{
numeric, an integer that specifies the size of the grid to solve the model with
}
  \item{verbose}{
logical, if set to true, the function provides the output of the pomdp solver in R console
}
}
\value{
The function returns a list containing the followings:
\item{belief}{a dataframe of all the belief states (rows); there is a column at the end that indicates which line (vector) provides the best value for the given belief state. Use \code{\link{belief}} function to get this dataframe directly}
\item{belief_proportions}{a dataframe that includes the probabilities of being in each actual state for each node of the policy graph (rows). Use \code{\link{belief.proportions}} function to get this dataframe directly}
\item{alpha}{a dataframe that includes all the coefficients of the optimal hyperplanes. Use \code{\link{alpha}} function to get this dataframe directly}
\item{pg}{a dataframe of all the nodes (rows) and arcs in the policy graph and how they are connected as well as the optimal action associated with each node in the graph. Use \code{\link{pg}} function to get this dataframe directly}
\item{total_expected_reward}{a numeric value indicating the total expected reward of the optimal solution. Use \code{\link{total.expected.reward}} function to get this value directly}
\item{initial_node}{an integer number indicating the initial node of the policy graph. Use \code{\link{initial.node}} function to get this node number directly}
\item{solver_output}{a character that contains the output of the 'pomdp-solve' including its iterations. Use \code{\link{solver.output}} function to get this output directly}
\item{model}{a list of all the inputs defined by the user but transformed into POMDP-friendly structure. Use \code{\link{model}} function to get this list directly}
}



\references{
[1] For further details on how the POMDP solver utilized in this R package works check the following website:
\url{www.pomdp.org} 

[2] Cassandra, A. Rocco, Exact and approximate algorithms for partially observable markov decision processes, (1998). 
\url{https://dl.acm.org/citation.cfm?id=926710}
}
\author{
Hossein Kamalzadeh, Michael Hahsler
}
\note{
\strong{'pomdp-solve'} program uses the basic dynamic programming approach, solving one stage at a time working backwards in time. It does finite horizon problems with or without discounting. It will stop solving if the answer is within a tolerable range of the infinite horizon answer, and there are a couple of different stopping conditions (requires a discount factor less than 1.0). Alternatively you can solve a finite horizon problem for some fixed horizon length.
}

\examples{
# below are examples and different ways of how you can define the input arguments of the function.
# you can find complete and executable examples such as tiger problem at the end of this section.

### different ways of input definition ################################################
discount <- 0.9
states <- c("state1" , "state2" , "state3")
actions <- c("action1" , "action2")
observations <- c("obs1" , "obs2")

## for the start vector, based on the options you can do either of the followings:
start <- c(0.5 , 0.3 , 0.2) # probabilities of being in each state
start <- "uniform" # equal probabilities for all the states
start <- 1 # initial state is "state2"
start <- "state2" # initial state is "state2"
start <- c("state1" , "state3") # uniform distribution over "state1" and "state3"
start <- c("-" , "state2") # uniform distribution over "state1" and "state3" ("state2" excluded)

## possible ways to define transition probabilities:
transition_prob <- data.frame("action" = c("action1" , "action1" , "action1" , 
                                            0 , 0 , "action1" , 0 , 0 , 0 ,
                                           "action2" , "action2" , 1 , 
                                           1 , 1 , 1 , 1 , 1 , 1),
                              "start-state" = c("state1" , "state1" , 0 , 
                                                "state2" , "state2" , "state2" , 
                                                "state3" , 2 , 2 ,
                                                "state1" , "state1" , 0 , 
                                                "state2" , "state2" , "state2" , 
                                                "state3" , 2 , 2),
                              "end-state" = c("state1" , "state2" , 2 , 
                                              "state1" , "state2" , "state3" , 
                                              "state1" , 1 , 2 ,
                                              "state1" , "state2" , 2 , 
                                              "state1" , "state2" , "state3" , 
                                              "state1" , 1 , 2),
                              "probability" = c(0.1 , 0.4 , 0.5 , 0 , 0.7 , 0.3 , 0.4 , 0.4 , 0.2,
                                                0 , 0.6 , 0.4 , 0.1 , 0.9 , 0 , 0.7 , 0.3 , 0))


transition_prob <- list("action1" = matrix(c( 0.1 , 0.4 , 0.5 ,
                                              0 , 0.7 , 0.3 , 
                                              0.4 , 0.4 , 0.2) , nrow = 3 , byrow = TRUE) ,
                        "action2" = matrix(c( 0 , 0.6 , 0.4 ,
                                              0.1 , 0.9 , 0 ,
                                              0.7 , 0.3 , 0 ) , nrow = 3 , byrow = TRUE))

transition_prob <- list("action1" = "identity" ,
                        "action2" = "uniform")

transition_prob <- list("action1" = matrix(c( 0.1 , 0.4 , 0.5 ,
                                              0 , 0.7 , 0.3 , 
                                              0.4 , 0.4 , 0.2) , nrow = 3 , byrow = TRUE) ,
                        "action2" = "uniform")
                                              
                                              
## possible ways to define observation probabilities:
observation_prob <- data.frame("action" = c( "*" , "*" , "*" , "*" , "*" , "*") , 
                               "end-state" = c("state1" , "state1" , "state2" , 
                                               "state2" , "state3" , "state3") ,
                               "observation" = c("obs1" , "obs2" , "obs1" , 
                                                 "obs2" , "obs1" , "obs2"),
                               "probability" = c(0.1 , 0.9 , 0.3 , 0.7 , 0.4 , 0.6))


observation_prob <- list("action1" = matrix(c(0.1 , 0.9 ,
                                              0.3 , 0.7 , 
                                              0.4 , 0.6) , nrow = 3 , byrow = TRUE) ,
                         "action2" = matrix(c(0.1 , 0.9 ,
                                              0.3 , 0.7 ,
                                              0.4 , 0.6 ) , nrow = 3 , byrow = TRUE))

observation_prob <- list("action1" = "uniform" ,
                        "action2" = "uniform")

observation_prob <- list("action1" = "uniform" ,
                         "action2" = matrix(c(0.1 , 0.9 ,
                                              0.3 , 0.7 ,
                                              0.4 , 0.6 ) , nrow = 3 , byrow = TRUE))

# as we see above, both observation probability matrices are the same and 
# it indicates that the observation probabilities are independent of the action taken. 
# Thus we can use the dataframe format with the help of "*" to make it more concise. 
# "*" here indicates that the probabilities are independent of the actions. 
# "*" could be used anytime where there is independency.

## possible ways to define the rewards:
reward <- data.frame("action" = c("action1" , "action1" , "action1" , 
                                  "action2" , "action2" , "action2") ,
                     "start-state" = c("*" , "*" , "*" , "*" , "*" , "*") ,
                     "end-state" = c("state1" , "state2" , "state3" , 
                                     "state1" , "state2" , "state3") ,
                     "observation" = c("*" , "*" , "*" , "*" , "*" , "*") ,
                     "reward" = c(10000 , 2000 , 50 , 150 , 2500 , 100))

## values:
values <- "cost"
values <- "reward"

## grid size:
grid_size <- 10 # the larger, the longer it takes to solve


### The Tiger example #################################################################
discount <- 0.75
values <- "reward"
states <- c("tiger-left" , "tiger-right")
actions <- c("listen" , "open-left" , "open-right")
observations <- c("tiger-left" , "tiger-right")
start <- "uniform"
grid_size <- 10
transition_prob <- list("listen" = "identity" , 
                        "open-left" = "uniform" , 
                        "open-right" = "uniform")
observation_prob <- list("listen" = matrix(c(0.85 , 0.15 ,
                                             0.15 , 0.85) , nrow = 2 , byrow = TRUE) , 
                         "open-left" = "uniform" ,
                         "open-right" = "uniform")
reward <- data.frame("action" = c("listen" , "open-left" , "open-left" , 
                                  "open-right" , "open-right") ,
                     "start-state" = c("*" , "tiger-left" , "tiger-right" , 
                                       "tiger-left" , "tiger-right") ,
                     "end-state" = c("*" , "*" , "*" , "*" , "*") ,
                     "observation" = c("*" , "*" , "*" , "*" , "*") ,
                     "reward" = c(-1 , -100 , 10 , 10 , -100))
result <- pomdp(discount, states, actions, observations, start, transition_prob, 
                observation_prob, reward, values = "reward", grid_size, verbose = FALSE)
result

}

