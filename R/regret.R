#' Calculate the Regret of a Policy
#'
#' Calculates the regret of a policy relative to a benchmark policy.
#' 
#' Calculates the regret defined as \eqn{J^{\pi^*}(b_0) - J^{\pi}(b_0)} with \eqn{J^\pi} representing the expected long-term
#' reward given the policy \eqn{\pi} and the start belief \eqn{b_0}. Note that for regret usually the optimal policy \eqn{\pi^*} is used as the benchmark.
#' Since the optimal policy may not be known, regret relative to the best known policy can be used.     
#'
#' @family POMDP
#'
#' @param policy to calculate the regret for. Can be a solved POMDP, a matrix containing a value_function (see value_function()), 
#'    or a policy (see [policy()]). 
#' @param benchmark a solved POMDP with the (optimal) policy. Regret is calculated relative to this
#'    policy.
#' @param belief the used start belief. If NULL then the start belief of the `benchmark` is used.  
#'
#' @return the regret as a difference of expected long-term rewards.
#'
#'
#' @author Michael Hahsler
#' @examples
#' data(Tiger)
#' 
#' sol_optimal <- solve_POMDP(Tiger)
#' sol_optimal
#' 
#' # perform exact value iteration for 10 epochs
#' sol_quick <- solve_POMDP(Tiger, method = "enum", horiz = 10)
#' sol_quick
#' 
#' regret(sol_quick, sol_optimal)
#' @export
regret <- function(policy, benchmark, belief = NULL) {
  if (!inherits(benchmark, "POMDP") || !is_solved_POMDP(benchmark))
    stop("benchmark needs to be a solved POMDP.")
    
  belief <- .translate_belief(belief, benchmark)
  if (is.null(belief))
    stop("belief needs to be specified if val_optimal is not a solved POMDP object with a start belief vector!")
    
  benchmark_fv <- value_function(benchmark)[[1]]
    
  if (inherits(policy, "POMDP"))
    policy_fv <- value_function(policy)[[1]]
  else if (is.data.frame(policy))
    policy_fv <- as.matrix(policy[[1]][, -ncol(policy[[1]]), drop = FALSE])
  else
    policy_fv <- policy
  
  
  r_bench <- reward_cpp(rbind(belief), benchmark_fv)$reward
  r_pol <- reward_cpp(rbind(belief), policy_fv)$reward
  
  r_bench - r_pol
}
