#' Add a Policy to a POMDP Problem Description
#'
#' Add a policy to a POMDP problem description allows the user to 
#' test policies on modified problem descriptions or to test manually created
#' policies.
#'
#' @family POMDP
#' @family MDP
#'
#' @param model a POMDP or MDP model description.
#' @param policy a policy data.frame.
#'
#' @return The model description with the added policy.
#'
#' @author Michael Hahsler
#' @examples
#' data(Tiger)
#' 
#' sol <- solve_POMDP(Tiger)
#' sol
#' 
#' # Example 1: Use the solution policy on a changed POMDP problem
#' #            where listening is perfect and simulate the expected reward
#' 
#' perfect_Tiger <- Tiger
#' perfect_Tiger$observation_prob <- list(
#'   listen = diag(1, length(perfect_Tiger$states), 
#'     length(perfect_Tiger$observations)), 
#'   `open-left` = "uniform",
#'   `open-right` = "uniform"
#' )
#' 
#' sol_perfect <- add_policy(perfect_Tiger, sol)
#' sol_perfect
#' 
#' simulate_POMDP(sol_perfect, n = 1000)$avg_reward
#' 
#' # Example 2: Handcraft a policy and apply it to the Tiger problem
#' 
#' # original policy
#' policy(sol)
#' plot_value_function(sol)
#' plot_belief_space(sol)
#' 
#' # create a policy manually where the agent opens a door at a believe of 
#' #  roughly 2/3 (note the alpha vectors do not represent 
#' #  a valid value function)
#' p <- list(
#' data.frame(
#'   `tiger-left` = c(1, 0, -2),
#'   `tiger-right` = c(-2, 0, 1), 
#'   action = c("open-right", "listen", "open-left"),
#'   check.names = FALSE
#' ))
#' p
#' 
#' custom_sol <- add_policy(Tiger, p)
#' custom_sol
#' 
#' policy(custom_sol)
#' plot_value_function(custom_sol)
#' plot_belief_space(custom_sol)
#' 
#' simulate_POMDP(custom_sol, n = 1000)$avg_reward
#' @export
add_policy <- function(model, policy) {
  UseMethod("add_policy")
}

#' @export
add_policy.POMDP <- function(model, policy) {
  if(inherits(policy, "POMDP"))
    policy <- policy(policy, drop = FALSE)
  
  solution <- list(
    alpha = lapply(policy, function(x) 
      as.matrix(x[ , -ncol(x), drop = FALSE])),
    pg = lapply(policy, FUN = function(x) 
      cbind(node = seq(nrow(x)), x[, ncol(x), drop = FALSE])),
    method = "manual",
    converged = FALSE
  )
  
  model$solution <- solution
  model <- check_and_fix_MDP(model)
   
  model
}

#' @export
add_policy.MDP <- function(model, policy) {
  if(inherits(policy, "MDP"))
    policy <- policy(policy)
  
  if(is.null(policy$U))
    policy$U <- MDP_policy_evaluation(policy, model)
  
  solution <- list(
    method = "manual",
    policy = list(policy),
    converged = NA
  )
  
  model$solution <- solution
  model <- check_and_fix_MDP(model)
  
  model
}


