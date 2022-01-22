#' Optimal action for a belief
#'
#' Determines the optimal action for a policy (solved POMDP) for a given belief
#' at a given epoch.
#'
#'
#' @param model a solved [POMDP].
#' @param belief The belief (probability distribution over the states) as a
#' vector or a matrix with multiple belief states as rows.
#' @param epoch what epoch of the policy should be used.
#' @return The name of the optimal action.
#' @author Michael Hahsler
#' @examples
#' data("Tiger")
#' Tiger
#'
#' sol <- solve_POMDP(model = Tiger)
#'
#' # these are the states
#' sol$model$states
#'
#' # belief that tiger is to the left
#' optimal_action(sol, c(1, 0))
#' optimal_action(sol, "tiger-left")
#'
#' # belief that tiger is to the right
#' optimal_action(sol, c(0, 1))
#' optimal_action(sol, "tiger-right")
#'
#' # belief is 50/50
#' optimal_action(sol, c(.5, .5))
#' optimal_action(sol, "uniform")
#'
#' # the POMDP is converged, so all epoch give the same result.
#' optimal_action(sol, "tiger-right", epoch = 10)
#'
#' @export
optimal_action <-
  function(model, belief, epoch = 1)
    reward(model, belief = belief, epoch = epoch)[["action"]]
