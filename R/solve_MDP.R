# TODO: deal with available actions for states actions(s)

#' Solve an MDP Problem
#'
#' A simple implementation of value iteration and modified policy iteration.
#'
#' @family solver
#' @family MDP
#' 
#' @param model a POMDP problem specification created with [POMDP()].
#'   Alternatively, a POMDP file or the URL for a POMDP file can be specified.
#' @param method string; one of the following solution methods: `'value'`,
#'   `'policy'`.
#' @param horizon an integer with the number of epochs for problems with a
#'   finite planning horizon. If set to `Inf`, the algorithm continues
#'   running iterations till it converges to the infinite horizon solution. If
#'   `NULL`, then the horizon specified in `model` will be used.  For
#'   time-dependent POMDPs a vector of horizons can be specified (see Details
#'   section).
#' @param discount discount factor in range \eqn{[0, 1]}. If `NULL`, then the
#'   discount factor specified in `model` will be used.
#' @param terminal_values a vector with terminal utilities for each state. If
#'   `NULL`, then a vector of all 0s is used.
#' @param eps maximum error allowed in the utility of any state
#'   (i.e., the maximum policy loss).
#' @param max_iterations maximum number of iterations allowed to converge. If the
#'   maximum is reached then the non-converged solution is returned with a
#'   warning.
#' @param k_backups number of look ahead steps used for approximate policy evaluation
#'   used by method `'policy'`.
#' @param verbose logical, if set to `TRUE`, the function provides the
#'   output of the pomdp solver in the R console.
#'
#' @return `solve_MDP()` returns an object of class POMDP which is a list with the
#'   model specifications (`model`), the solution (`solution`).
#'   The solution is a list with the elements:
#'   - `policy` a list representing the policy graph. The list only has one element for converged solutions.
#'   - `converged` did the algorithm converge (`NA`) for finite-horizon problems.
#'   - `delta` final delta (infinite-horizon only)
#'   - `iterations` number of iterations to convergence (infinite-horizon only)
#'   
#'
#' @author Michael Hahsler
#' @examples
#' data(Maze)
#' Maze
#'
#' # use value iteration
#' maze_solved <- solve_MDP(Maze, method = "value")
#' policy(maze_solved)
#'
#' # value function (utility function U)
#' plot_value_function(maze_solved)
#' 
#' # Q-function (states times action)
#' q_values_MDP(maze_solved)
#'
#' # use modified policy iteration
#' maze_solved <- solve_MDP(Maze, method = "policy")
#' policy(maze_solved)
#'
#' # finite horizon
#' maze_solved <- solve_MDP(Maze, method = "value", horizon = 3)
#' policy(maze_solved)
#'
#' # create a random policy where action n is very likely and approximate 
#' #  the value function. We change the discount factor to .9 for this.
#' Maze_discounted <- Maze
#' Maze_discounted$discount <- .9
#' pi <- random_MDP_policy(Maze_discounted, prob = c(n = .7, e = .1, s = .1, w = 0.1))
#' pi
#' 
#' # compare the utility function for the random policy with the function for the optimal 
#' #  policy found by the solver.
#' maze_solved <- solve_MDP(Maze)
#' 
#' approx_MDP_policy_evaluation(pi, Maze, k_backup = 100)
#' approx_MDP_policy_evaluation(policy(maze_solved)[[1]], Maze, k_backup = 100)
#' 
#' # Note that the solver already calculates the utility function and returns it with the policy
#' policy(maze_solved)
#' @export
solve_MDP <- function(model,
  horizon = NULL,
  discount = NULL,
  terminal_values = NULL,
  method = "value",
  eps = 0.01,
  max_iterations = 1000,
  k_backups = 10,
  verbose = FALSE) {
  if (!inherits(model, "MDP"))
    stop("'model' needs to be of class 'MDP'.")
  
  methods <- c("value", "policy")
  method <- match.arg(method, methods)
  
  ### default is infinite horizon
  if (!is.null(horizon))
    model$horizon <- horizon
  if (is.null(model$horizon))
    model$horizon <- Inf
  
  if (!is.null(discount))
    model$discount <- discount
  if (is.null(model$discount)) {
    message("No discount rate specified. Using .9!")
    model$discount <- .9
  }
  
  if (is.infinite(model$horizon))
    switch(
      method,
      value = MDP_value_iteration_inf_horizon(model, eps, max_iterations,
        U = terminal_values, verbose = verbose),
      policy = MDP_policy_iteration_inf_horizon(
        model,
        eps,
        max_iterations,
        k_backups,
        U = terminal_values,
        verbose = verbose
      )
    )
  else
    switch(
      method,
      value = MDP_value_iteration_finite_horizon(
        model,
        horizon = model$horizon,
        U = terminal_values,
        verbose = verbose
      ),
      policy = stop("Not implemented yet.")
    )
  
}

# Q-Function:
# the (optimal) state-action value function Q_s(a,k) is the expected total reward
# from stage k onward, if we choose a_k = a and then proceed optimally (given by U).
.QV <-
  function(s, a, P, R, GAMMA, U)
    sum(P[[a]][s, ] * (R[[a]][[s]] + GAMMA * U))

.QV_vec <- Vectorize(.QV, vectorize.args = c("s", "a"))

#' @rdname solve_MDP
#' @param U a vector with state utilities (expected sum of discounted rewards from that point on).
#' @return `q_values_MDP()` returns a state by action matrix specifying the Q-function, 
#'   i.e., the utility value of executing each action in each state.
#' @export
q_values_MDP <- function(model, U = NULL) {
  if (!inherits(model, "MDP"))
    stop("'model' needs to be of class 'MDP'.")
  
  S <- model$states
  A <- model$actions
  P <- transition_matrix(model)
  R <- reward_matrix(model)
  policy <- model$solution$policy[[1]]
  GAMMA <- model$discount
  
  if (is.null(U)) 
    if (!is.null(policy))
      U <- policy$U
    else
      stop("'model' does not contain state utilities (it is unsolved). You need to specify U.")
  
  qs <- outer(S, A, .QV_vec, P, R, GAMMA, U)
  dimnames(qs) <- list(S, A)
  qs
  }

# TODO: we could check for convergence
MDP_value_iteration_finite_horizon <-
  function(model,
    horizon,
    U = NULL,
    verbose = FALSE) {
    if (!inherits(model, "MDP"))
      stop("'model' needs to be of class 'MDP'.")
    
    S <- model$states
    A <- model$actions
    P <- transition_matrix(model)
    R <- reward_matrix(model)
    GAMMA <- model$discount
    
    horizon <- as.integer(horizon)
    
    if (is.null(U))
      U <- rep(0, times = length(S))
    names(U) <- S
    
    policy <- list()
    
    for (t in seq(horizon, 1)) {
      if (verbose)
        cat("Iteration for t = ", t)
      
      Qs <- outer(S, A, .QV_vec, P, R, GAMMA, U)
      m <- apply(Qs, MARGIN = 1, which.max)
      
      pi <- A[m]
      U <- Qs[cbind(seq_along(S), m)]
      
      policy[[t]] <- data.frame(
        state = S,
        U = U,
        action = pi,
        row.names = NULL
      )
    }
    
    model$solution <- list(policy = policy,
      converged = NA)
    model
  }

MDP_value_iteration_inf_horizon <-
  function(model,
    eps,
    max_iterations = 1000,
    U = NULL,
    verbose = FALSE) {
    S <- model$states
    A <- model$actions
    P <- transition_matrix(model)
    R <- reward_matrix(model)
    GAMMA <- model$discount
    
    if (is.null(U))
      U <- rep(0, times = length(S))
    names(U) <- S
    
    converged <- FALSE
    for (i in seq_len(max_iterations)) {
      if (verbose)
        cat("Iteration:", i)
      
      Qs <- outer(S, A, .QV_vec, P, R, GAMMA, U)
      m <- apply(Qs, MARGIN = 1, which.max)
      
      pi <- A[m]
      U_t_minus_1 <- Qs[cbind(seq_along(S), m)]
      
      delta <- max(abs(U_t_minus_1 - U))
      U <- U_t_minus_1
      
      if (verbose)
        cat(" -> delta:", delta, "\n")
      
      if (delta <= eps * (1 - GAMMA) / GAMMA) {
        converged <- TRUE
        break
      }
    }
    
    if (verbose)
      cat("Iterations needed:", i, "\n")
    
    if (!converged)
      warning(
        "MDP solver did not converge after ",
        i,
        " iterations (delta = ",
        delta,
        ").",
        " Consider decreasing the 'discount' factor or increasing 'eps' or 'max_iterations'."
      )
    
    model$solution <- list(
      method = "value iteration",
      policy = list(data.frame(
        state = S,
        U = U,
        action = pi,
        row.names = NULL
      )),
      converged = converged,
      delta = delta,
      iterations = i
    )
    model
  }

# Policy iteration with approximate policy evaluation

#' @rdname solve_MDP
#' @param prob probability vector for actions. 
#' @return `random_MDP_policy()` returns a data.frame with columns state and action to define a policy.
#' @export
random_MDP_policy <-
  function(model, prob = NULL) {
    if (!inherits(model, "MDP"))
      stop("'model' needs to be of class 'MDP'.")
    
    data.frame(
      state = model$states,
      action = sample(
        model$actions,
        size = length(model$states),
        replace = TRUE,
        prob = prob
      )
    )
  }

#' @rdname solve_MDP
#' @param pi a policy as a data.frame with columns state and action.
#' @return `approx_MDP_policy_evaluation()` is used by the modified policy 
#'   iteration algorithm and returns an approximate utility vector U estimated by evaluating policy `pi`.
#' @export
approx_MDP_policy_evaluation <- function(pi, model, U = NULL, k_backups = 10) {
  if (!inherits(model, "MDP"))
    stop("'model' needs to be of class 'MDP'.")
  
  S <- model$states
  A <- model$actions
  P <- transition_matrix(model)
  R <- reward_matrix(model)
  GAMMA <- model$discount
  
  if (is.data.frame(pi))
    pi <- pi$action
  names(pi) <- S
  
  # start with all 0s if no previous U is given
  if (is.null(U))
    U <- rep(0, times = length(S))
  
  for (i in seq_len(k_backups))
    U <- .QV_vec(S, pi, P, R, GAMMA, U)
  
  U
}

MDP_policy_iteration_inf_horizon <-
  function(model,
    eps,
    max_iterations = 1000,
    k_backups = 10,
    U = NULL,
    verbose = FALSE) {
    S <- model$states
    A <- model$actions
    P <- transition_matrix(model)
    R <- reward_matrix(model)
    GAMMA <- model$discount
    
    if (is.null(U))
      U <- rep(0, times = length(S))
    
    pi <- random_MDP_policy(model)$action
    names(pi) <- S
    
    converged <- FALSE
    for (i in seq_len(max_iterations)) {
      if (verbose)
        cat("Iteration ", i, "\n")
      
      # evaluate to get U from pi
      U <- approx_MDP_policy_evaluation(pi, model, U, k_backups = k_backups)
      
      # get greedy policy for U
      Qs <- outer(S, A, .QV_vec, P, R, GAMMA, U)
      m <- apply(Qs, MARGIN = 1, which.max)
      pi_prime <- A[m]
      
      if (all(pi == pi_prime)) {
        converged <- TRUE
        break
      }
      
      pi <- pi_prime
    }
    
    if (!converged)
      warning(
        "MDP solver did not converge after ",
        i,
        " iterations.",
        " Consider decreasing the 'discount' factor or increasing 'max_iterations'."
      )
    
    model$solution <- list(
      method = "policy iteration",
      policy = list(data.frame(
        state = S,
        U = U,
        action = pi,
        row.names = NULL
      )),
      converged = converged,
      iterations = i
    )
    model
  }
