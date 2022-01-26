#' Solve an MDP Problem
#'
#' A simple implementation of value iteration and modified policy iteration.
#'
#' @param model a POMDP problem specification created with [POMDP()].
#'   Alternatively, a POMDP file or the URL for a POMDP file can be specified.
#' @param eps maximum error allowed in the utility of any state 
#'   (i.e., the maximum policy loss).
#' @param horizon an integer with the number of epochs for problems with a
#'   finite planning horizon. If set to `Inf`, the algorithm continues
#'   running iterations till it converges to the infinite horizon solution. If
#'   `NULL`, then the horizon specified in `model` will be used.  For
#'   time-dependent POMDPs a vector of horizons can be specified (see Details
#'   section).
#' @param discount discount factor in range \eqn{[0, 1]}. If `NULL`, then the
#'   discount factor specified in `model` will be used.
#' @param verbose logical, if set to `TRUE`, the function provides the
#'   output of the pomdp solver in the R console.
#' @param max_iterations maximum number of iterations to converge. If the 
#'   maximum is reached then the non-converged solution is returned with a 
#'   warning.   
#' @param method string; one of the following solution methods: `"value"`,
#'   `"policy"`.
#' 
#' @return The solver returns an object of class POMDP which is a list with the
#'   model specifications (`model`), the solution (`solution`).
#'   
#' @author Michael Hahsler
#' @examples
#' data(Maze) 
#' Maze
#' 
#' maze_solved <- solve_MDP(Maze, method = "value")
#' policy(maze_solved)
#' @export
solve_MDP <- function(model,
  eps = 0.01,
  horizon = NULL,
  discount = NULL,
  max_iterations = 1000,
  method = "value",
  verbose = FALSE) {
  
  methods <- c("value", "policy")
  method <- match.arg(method, methods)
  
  if (is.null(horizon))
    model$model$horizon <- horizon 
  
  if (!is.null(discount))
    model$model$discount <- discount 
  if (is.null(model$model$discount)) {
    message("No discount rate specified. Using .9!")
    model$model$discount <- .9
  }
 
  switch(method,
    value = MDP_value_iteration(model, eps, max_iterations, verbose = verbose), 
    policy = stop("not implemented yet!")
  )
}

.QV <- function(s, a, P, R, GAMMA, U) sum(P[[a]][s, ] * (R[[a]][[s]] + GAMMA * U))

MDP_value_iteration <- function(model, eps, max_iterations = 1000, verbose = FALSE) {
  S <- model$model$states
  A <- model$model$actions
  P <- transition_matrix(model)
  R <- reward_matrix(model)
  GAMMA <- model$model$discount
  
  U_prime <- rep(0, times = length(S))
  names(U_prime) <- S
  
  i <- 1L
  converged <- NA
  while (TRUE) {
    if(verbose) cat("Iteration:", i)
    delta <- 0
    U <- U_prime
    
    for (s in S) {
      U_prime[s] <- max(sapply(
        A,
        FUN = function(a) {
          # Q-Value
          sum(P[[a]][s, ] * (R[[a]][[s]] + GAMMA * U))
        }
      ))
      delta <- max(delta, abs(U_prime[s] - U[s]))
    }
    if(verbose) cat(" -> delta:", delta, "\n")
    
    if (delta <= eps * (1 - GAMMA) / GAMMA) {
      converged <- TRUE 
      break
    }
    
    i <- i + 1L
    if (i >= max_iterations) {
      converged <- FALSE 
      break
      }
  }
  
  if (verbose) cat("Iterations needed:", i, "\n")
  
  pi_star <- A[sapply(
    S,
    FUN = function(s)
      which.max(sapply(
        A,
        FUN = function(a) {
          # Q-Value
          sum(P[[a]][s, ] * (R[[a]][[s]] + GAMMA * U))
        }
      ))
  )]
  
  if (!converged)
    warning("MDP solver did not converge after ", i, " iterations (delta = ", delta, ").",
      " Consider decreasing the 'discount' factor or increasing 'eps' or 'max_iterations'.")
  
  model$solution <- list(
    policy = list(data.frame(state = S, U = U, action = pi_star, row.names = NULL)), 
    converged = converged, delta = delta, iterations = i)
  model
}

### FIXME: finite horizon!!!
