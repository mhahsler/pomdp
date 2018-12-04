\name{belief.proportions}
\alias{belief.proportions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rerurn belief proportions of a POMDP policy graph
}
\description{
The function simply returns the proportions (probabilities) of being in each actual state for each node of a policy graph
}
\usage{
belief.proportions(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP
}
}

\value{
returns a dataframe that includes the probabilities of being in each actual state for each node of the policy graph (rows)
}

\seealso{
\code{\link{pomdp}}
}
\examples{
# you need to use pomdp function to generate an object of class POMDP first.
# here for an example we use the Tiger example code to generate such an object.
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
                
# then using the belief.proportions() function we print all the belief proportions of the given 
# POMDP object

belief.proportions(result)
}