\name{TigerProblem}
\alias{TigerProblem}
\docType{data}
\title{Tiger Problem POMDP Data}
\description{
The model for the Tiger Problem.
}
\usage{data("TigerProblem")}
\format{
  A list of eight elements:
 discount,  
 states,     
 actions,    
 observations,    
 start,           
 transition_prob, 
 observation_prob,
 reward.
}
\details{
You are standing in front of two closed doors, behind one there is a tiger, and behind the other door, there is a treasure. If you open the door behind which the tiger is, you will get hurt by the tiger (bodily damage, negative rewards) and if you open the door to the treasure, you will be rewarded with the treasure (positive rewards). Instead of opening the doors, you also have the option to wait and listen for tiger noises. But listening is neither free nor entirely accurate. You might hear the tiger behind the left door while it is actually behind the right door and vice versa.

The tiger will be put behind one of the doors with equal probability.

States of the system are tiger behind the left door (tiger-left) and a tiger behind the right door (tiger-right). 

Available actions are: open the left door (open-left), open the right door (open-right) or to listen (listen). 

Rewards associated with these actions depending on what state you end in are +10 for opening the correct door (the door with treasure), -100 for opening the door behind which the tiger is and -1 for just listening. 

As a result of listening, there are two observations: either you hear the tiger on the right (tiger-right), or you hear it on the left (tiger-left). 

To create the policy, the problem will reset as soon as you open one of the doors.
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Anthony R. Cassandra, Leslie P Kaelbling, and Michael L. Littman (1994). Acting Optimally in Partially Observable Stochastic Domains. In Proceedings of the Twelfth National Conference on Artificial Intelligence, pp. 1023-1028. 
}
\examples{
data(TigerProblem)

TigerProblem
}
\keyword{datasets}
