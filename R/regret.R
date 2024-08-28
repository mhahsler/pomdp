#' Calculate the Regret of a Policy
#'
#' Calculates the regret of a policy relative to a benchmark policy.
#' 
#' Regret is defined as \eqn{V^{\pi^*}(s_0) - V^{\pi}(s_0)} with \eqn{V^\pi} representing the expected long-term
#' state value (represented by the value function) given the policy \eqn{\pi} and the start 
#' state \eqn{s_0}. For POMDPs the start state is the start belief \eqn{b_0}. 
#' 
#' Note that for regret usually the optimal policy \eqn{\pi^*} is used as the benchmark.
#' Since the optimal policy may not be known, regret relative to the best known policy can be used.     
#'
#' @family POMDP
#'
#' @param policy a solved POMDP containing the policy to calculate the regret for. 
#' @param benchmark a solved POMDP with the (optimal) policy. Regret is calculated relative to this
#'    policy.
#' @param start the used start (belief) state. If NULL then the start (belief) state of the `benchmark` is used.  
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
#' sol_quick <- solve_POMDP(Tiger, method = "enum", horizon = 10)
#' sol_quick
#' 
#' regret(sol_quick, benchmark = sol_optimal)
#' @export
regret <- function(policy, benchmark, start = NULL) {
  UseMethod("regret")
}

#' @export
regret.POMDP <- function(policy, benchmark, start = NULL) {
  if (!inherits(benchmark, "POMDP") || !is_solved_POMDP(benchmark))
    stop("benchmark needs to be a solved POMDP.")
  
  if (!inherits(policy, "POMDP") || !is_solved_POMDP(policy))
    stop("policy needs to be a solved POMDP.")
  
  start <- .translate_belief(start, benchmark)
  if (is.null(start))
    stop("start belief needs to be specified if val_optimal is not a solved POMDP object with a start belief vector!")
    
  r_bench <- reward_cpp(benchmark, rbind(start))$reward
  r_pol <- reward_cpp(policy, rbind(start))$reward
  
  r_bench - r_pol
}

