#' Russian Tiger Problem POMDP Specification
#'
#' This is a variation of the Tiger Problem introduced in Cassandra et al (1994)
#' with an absorbing state after a door is opened.
#'
#' The original Tiger problem is available as [Tiger]. The original problem is
#' an infinite-horizon problem, where when the agent opens a door then the 
#' problem starts over. The infinite-horizon problem can be solved if 
#' a discount factor \eqn{\gamma < 1} is used.
#' 
#' The Russian Tiger problem uses no discounting, but instead
#' adds an absorbing state `done`  which is reached 
#' after the agent opens a door. It adds the action `nothing` to indicate
#' that the agent does nothing. The `nothing` action is only available in the 
#' state `done` indicated by a reward of `-Inf` from all after states. A new 
#' observation `done` is only emitted by the state `done`. Also, the Russian
#' tiger inflicts more pain with a negative reward of -1000.
#'
#' @name RussianTiger
#' @aliases RussianTiger
#' @family POMDP_examples
#' @docType data
#' @format An object of class [POMDP].
#' @keywords datasets
#' @examples
#' data("RussianTiger")
#' RussianTiger
#'
#' # states, actions, and observations
#' RussianTiger$states  
#' RussianTiger$actions 
#' RussianTiger$observations
#' 
#' # reward (-Inf indicates unavailable actions)
#' RussianTiger$reward
#'
#' sapply(RussianTiger$states, FUN = function(s) actions(RussianTiger, s))
#' 
#' plot_transition_graph(RussianTiger, vertex.size = 30, edge.arrow.size = .3, margin = .5)
#' 
#' # absorbing states
#' absorbing_states(RussianTiger)
#' 
#' # solve the problem.
#' sol <- solve_POMDP(RussianTiger)
#' policy(sol)
#' plot_policy_graph(sol)
NULL
