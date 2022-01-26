#' Tiger Problem POMDP Specification
#'
#' The model for the Tiger Problem introduces in Cassandra et al (1994).
#'
#' The original Tiger problem was published in Cassandra et al (1994). 
#' A tiger is put with equal
#' probability behind one of two doors represented by the states tiger-left and
#' tiger-right, while treasure is put behind the other door. You are standing
#' in front of the two closed doors and need to decide which one to open. If
#' you open the door with the tiger, you will get hurt by the tiger (a negative
#' reward of -100), but if you open the door with the treasure, you receive a
#' positive reward of 10. Instead of opening a door right away, you also have
#' the option to wait and listen for tiger noises producing an observation
#' (tiger-left or tiger-right). But listening is neither free (reward of -1)
#' nor entirely accurate. You might hear the tiger behind the left door while
#' it is actually behind the right door and vice versa. Once you open a door
#' (actions open-left or open-right), you receive the appropriate reward and
#' the problem is reset (i.e., the tiger is randomly assigned to a door and the
#' belief is set to 50/50).
#'
#' The three doors problem is an extension of the Tiger problem where the tiger
#' is behind one of three doors represented by three states (tiger-left,
#' tiger-center, and tiger-right) and treasure is behind the other two doors.
#' There are also three observations for listening.
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
#'
#' data("Tiger")
#' Tiger
#'
#' Tiger$model
#'
#' data("Three_doors")
#' Three_doors
#'
#' Three_doors$model
NULL
