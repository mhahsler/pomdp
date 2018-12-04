\name{pg}
\alias{pg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return policy graph of a POMDP solution
}
\description{
The function returns all the nodes and arcs of a policy graph of a POMDP solution in a table
}
\usage{
pg(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP
}
}

\value{
returns a dataframe of all the nodes (rows) and arcs in the policy graph and how they are connected as well as the optimal action associated with each node in the graph
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
                
# then using the pg() function we print the policy graph of the given POMDP object as a table

pg(result)
}
