#' Available Actions
#'
#' Determine the set of actions available in a state.
#'
#' Unavailable actions are modeled here a actions that have an immediate 
#'  reward of `-Inf` in the reward function.
#' @name actions
#' @family MDP
#' @family POMDP
#'
#' @param x a `MDP` pr `POMDP` object.
#' @param state a character vector of length one specifying the state.
#' @returns a character vector with the available actions.
#'
#' @author Michael Hahsler
#' @examples
#' data(RussianTiger)
#'
#' # The normal actions are "listen", "open-left", and "open-right".
#' # In the state "done" only the action "nothing" is available. 
#' 
#' actions(RussianTiger, state = "tiger-left")
#' actions(RussianTiger, state = "tiger-right")
#' actions(RussianTiger, state = "done")
#' @returns a vector with the available actions.
#' @export
actions <- function(x, state) {
  x$actions[!sapply(x$actions, FUN = function(a) { 
    all(reward_matrix(x, action = a, start.state = state) == -Inf) 
    })]
}
