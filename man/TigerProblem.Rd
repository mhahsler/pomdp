\name{TigerProblem}
\alias{TigerProblem}
\docType{data}
\title{Tiger Problem POMDP Specification}
\description{
The model for the Tiger Problem [1].
}
\usage{data("TigerProblem")}
\format{
  A list with the elements:
 discount,  
 states,     
 actions,    
 observations,    
 start,           
 transition_prob, 
 observation_prob,
 reward,
 name.
}
\details{
The Tiger Problem is defined as follows [1].
A tiger is put with equal probability behind one of two doors, while treasure is put behind the other one. You are standing in front of the two closed doors and need to decide which one to open. If you open the door with the tiger, you will get hurt by the tiger (negative reward), but if you open the door with the treasure, you receive a positive reward. Instead of opening a door right away, you also have the option to wait and listen for tiger noises. But listening is neither free nor entirely accurate. You might hear the tiger behind the left door while it is actually behind the right door and vice versa.

The states of the system are tiger behind the left door (tiger-left) and tiger behind the right door (tiger-right). 

Available actions are: open the left door (open-left), open the right door (open-right) or to listen (listen). 

Rewards associated with these actions depend on the resulting state: +10 for opening the correct door (the door with treasure), -100 for opening the door with the tiger. A reward of -1 is the cost of listening. 

As a result of listening, there are two observations: either you hear the tiger on the right (tiger-right), or you hear it on the left (tiger-left). 

The transition probability matrix for the action listening is identity, i.e., the
position of the tiger does not change. Opening either door means that the game restarts
by placing the tiger uniformly behind one of the doors.
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
[1] Anthony R. Cassandra, Leslie P Kaelbling, and Michael L. Littman (1994). Acting Optimally in Partially Observable Stochastic Domains. In Proceedings of the Twelfth National Conference on Artificial Intelligence, pp. 1023-1028. 
}
\examples{
data(TigerProblem)
TigerProblem

# solve the problem and look at the optimal policy graph (as a table and as a plot)
sol <- solve_POMDP(TigerProblem)
sol

solution(sol)$pg
plot(sol)
}
\keyword{datasets}
