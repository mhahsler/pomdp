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
#' @family MDP
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
  .validate_class(benchmark, "benchmark", "POMDP")
  if (!is_solved_POMDP(benchmark))
    stop("`benchmark` must be a solved object of class \"POMDP\".", call. = FALSE)
  
  .validate_class(policy, "policy", "POMDP")
  if (!is_solved_POMDP(policy))
    stop("`policy` must be a solved object of class \"POMDP\".", call. = FALSE)
  
  start <- .translate_belief(start, benchmark)
  if (is.null(start))
    stop("start belief needs to be specified if val_optimal is not a solved POMDP object with a start belief vector!")
    
  r_bench <- reward_cpp(benchmark, rbind(start))$reward
  r_pol <- reward_cpp(policy, rbind(start))$reward
  
  r_bench - r_pol
}

#' @export
regret.MDP <- function(policy, benchmark, start = NULL) {
  .validate_class(benchmark, "benchmark", "MDP")
  if (!is_solved_MDP(benchmark))
    stop("`benchmark` must be a solved object of class \"MDP\".", call. = FALSE)
  
  .validate_class(policy, "policy", "MDP")
  if (!is_solved_MDP(policy))
    stop("`policy` must be a solved object of class \"MDP\".", call. = FALSE)
  
  if (is.null(start))
    start <- which(start_vector(benchmark) == 1) 

  if (!is.null(start)) {
    start <- .match_model_value(start, benchmark$states, "start state")
    start <- match(start, benchmark$states)
  }
  
  if (length(start) != 1L)
    stop("A single start state needs to be specified!")
  
  r_bench <- policy(benchmark)$U[start]
  r_pol <- policy(policy)$U[start]
  
  r_bench - r_pol
}
