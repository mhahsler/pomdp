\name{plot}
\alias{plot.POMDP}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot POMDP policy graph
}
\description{
The function plots the POMDP policy graph provided in an object of classs POMDP
}
\usage{
\method{plot}{POMDP}(x, y = NULL, states = NULL, plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class POMDP
}
  \item{y}{
default is NULL
}
  \item{states}{
a vector of numbers indicating which states to be represented in the graph
}
  \item{plot}{
a logical that indicates the plot should be printed or not
}
  \item{\dots}{
other ploting options
}
}
\details{
x should be an object of class POMDP. \code{\link{pomdp}} function returns such an object. 
}
\value{
the function returns a POMDP policy graph with legends for states and observation arcs
}

\seealso{
\code{\link{pomdp}}
}

\examples{
# you need to use pomdp function to generate an object of class POMDP.
# here for an example we use the Tiger example code to generate the graph.
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
plot(result)
}

