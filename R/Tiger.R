#' Tiger Problem POMDP Specification
#'
#' The model for the Tiger Problem introduces in Cassandra et al (1994).
#'
#' The original Tiger problem was published in Cassandra et al (1994) as
#' follows: 
#' 
#' An agent is facing two closed doors and a tiger is put with equal
#' probability behind one of the two doors represented by the states
#' `tiger-left` and `tiger-right`, while treasure is put behind the other door.
#' The possible actions are `listen` for tiger noises or opening a door (actions
#' `open-left` and `open-right`). Listening is neither free (the action has a
#' reward of -1)  nor is it entirely accurate. There is a 15\% observation
#' probability that the agent hears the tiger behind the left door while it is
#' actually behind the right door and vice versa. If the agent opens  door with
#' the tiger, it will get hurt (a negative reward of -100), but if it opens the
#' door with the treasure, it will receive a positive reward of 10. After a door
#' is opened, the problem is reset(i.e., the tiger is randomly assigned to a
#' door with chance 50/50) and the the agent gets another try.
#' 
#' The three doors problem is an extension of the Tiger problem where the tiger
#' is behind one of three doors represented by three states (`tiger-left`,
#' `tiger-center`, and `tiger-right`) and treasure is behind the other two
#' doors. There are also three open actions and three different observations for
#' listening.
#'
#' @name Tiger
#' @aliases Tiger Three_doors
#' @docType data
#' @format An object of class [POMDP].
#' @references Anthony R. Cassandra, Leslie P Kaelbling, and Michael L.
#' Littman (1994). Acting Optimally in Partially Observable Stochastic Domains.
#' In Proceedings of the Twelfth National Conference on Artificial
#' Intelligence, pp. 1023-1028.
#' @keywords datasets
#' @examples
#' data("Tiger")
#' Tiger
#'
#' data("Three_doors")
#' Three_doors
NULL
