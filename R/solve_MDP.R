# TODO: deal with available actions for states actions(s)

#' Solve an MDP Problem
#'
#' Implementation of value iteration, modified policy iteration and other
#' methods based on reinforcement learning techniques to solve finite
#' state space MDPs.
#'
#' Implemented are the following dynamic programming methods (following
#' Russell and Norvig, 2010):
#'
#' * **Modified Policy Iteration**
#' starts with a random policy and iteratively performs
#' a sequence of
#'   1. approximate policy evaluation (estimate the value function for the
#' current policy using `k_backups` and function [`MDP_policy_evaluation()`]), and
#'   2. policy improvement (calculate a greedy policy given the value function).
#' The algorithm stops when it converges to a stable policy (i.e., no changes 
#' between two iterations).
#'
#' * **Value Iteration** starts with
#'   an arbitrary value function (by default all 0s) and iteratively
#'   updates the value function for each state using the Bellman equation. 
#'   The iterations
#'   are terminated either after `N_max` iterations or when the solution converges. 
#'   Approximate convergence is achieved 
#'   for discounted problems (with \eqn{\gamma < 1}) 
#'   when the maximal value function change for any state \eqn{\delta} is
#'   \eqn{\delta \le error (1-\gamma) / \gamma}. It can be shown that this means 
#'   that no state value is more than
#'   \eqn{error} from the value in the optimal value function. For undiscounted 
#'   problems, we use \eqn{\delta \le error}.
#'
#'   The greedy policy
#'   is calculated from the final value function. Value iteration can be seen as
#'   policy iteration with truncated policy evaluation.
#'   
#' Note that the policy converges earlier than the value function.  
#'
#' Implemented are the following temporal difference control methods
#' described in Sutton and Barto (2020).
#' Note that the MDP transition and reward models are only used to simulate
#' the environment for these reinforcement learning methods.
#' The algorithms use a step size parameter \eqn{\alpha} (learning rate) for the
#' updates and the exploration parameter \eqn{\epsilon} for
#' the \eqn{\epsilon}-greedy policy.
#' 
#' If the model has absorbing states to terminate episodes, then no maximal episode length
#' (`horizon`) needs to 
#' be specified. To make sure that the algorithm does finish in a reasonable amount of time,
#' episodes are stopped after 10,000 actions with a warning. For models without absorbing states,
#' a episode length has to be specified via `horizon`. 
#' 
#' * **Q-Learning** is an off-policy temporal difference method that uses
#'    an \eqn{\epsilon}-greedy behavior policy and learns a greedy target
#'    policy.
#'
#' * **Sarsa** is an on-policy method that follows and learns 
#'    an \eqn{\epsilon}-greedy policy. The final \eqn{\epsilon}-greedy policy
#'    is converted into a greedy policy.
#'
#' * **Expected Sarsa**: We implement an on-policy version that uses
#'   the expected value under the current policy for the update.
#'   It moves deterministically in the same direction as Sarsa
#'   moves in expectation. Because it uses the expectation, we can
#'   set the step size \eqn{\alpha} to large values and even 1.
#'
#' @family solver
#' @family MDP
#'
#' @param model an MDP problem specification.
#' @param method string; one of the following solution methods: `'value_iteration'`,
#'   `'policy_iteration'`, `'q_learning'`, `'sarsa'`, or `'expected_sarsa'`.
#' @param horizon an integer with the number of epochs for problems with a
#'   finite planning horizon. If set to `Inf`, the algorithm continues
#'   running iterations till it converges to the infinite horizon solution. If
#'   `NULL`, then the horizon specified in `model` will be used.
#' @param discount discount factor in range \eqn{(0, 1]}. If `NULL`, then the
#'   discount factor specified in `model` will be used.
#' @param verbose logical, if set to `TRUE`, the function provides the
#'   output of the solver in the R console.
#' @param ... further parameters are passed on to the solver function.
#'
#' @return `solve_MDP()` returns an object of class POMDP which is a list with the
#'   model specifications (`model`), the solution (`solution`).
#'   The solution is a list with the elements:
#'   - `policy` a list representing the policy graph. The list only has one element for converged solutions.
#'   - `converged` did the algorithm converge (`NA`) for finite-horizon problems.
#'   - `delta` final \eqn{\delta} (value iteration and infinite-horizon only)
#'   - `iterations` number of iterations to convergence (infinite-horizon only)
#'
#' @author Michael Hahsler
#' @references 
#' Russell, S., Norvig, P. (2021). Artificial Intelligence: A Modern Approach. 
#' Fourth edition. Prentice Hall.
#' 
#' Sutton, R. S., Barto, A. G. (2020). Reinforcement Learning: An Introduction. 
#' Second edition. The MIT Press.
#' 
#' @examples
#' data(Maze)
#' Maze
#'
#' # use value iteration
#' maze_solved <- solve_MDP(Maze, method = "value_iteration")
#' maze_solved
#' policy(maze_solved)
#'
#' # plot the value function U
#' plot_value_function(maze_solved)
#'
#' # Maze solutions can be visualized
#' gridworld_plot_policy(maze_solved)
#'
#' # use modified policy iteration
#' maze_solved <- solve_MDP(Maze, method = "policy_iteration")
#' policy(maze_solved)
#'
#' # finite horizon
#' maze_solved <- solve_MDP(Maze, method = "value_iteration", horizon = 3)
#' policy(maze_solved)
#' gridworld_plot_policy(maze_solved, epoch = 1)
#' gridworld_plot_policy(maze_solved, epoch = 2)
#' gridworld_plot_policy(maze_solved, epoch = 3)
#'
#' # create a random policy where action n is very likely and approximate
#' #  the value function. We change the discount factor to .9 for this.
#' Maze_discounted <- Maze
#' Maze_discounted$discount <- .9
#' pi <- random_MDP_policy(Maze_discounted, 
#'         prob = c(n = .7, e = .1, s = .1, w = 0.1))
#' pi
#'
#' # compare the utility function for the random policy with the function for the optimal
#' #  policy found by the solver.
#' maze_solved <- solve_MDP(Maze)
#'
#' MDP_policy_evaluation(pi, Maze, k_backup = 100)
#' MDP_policy_evaluation(policy(maze_solved), Maze, k_backup = 100)
#'
#' # Note that the solver already calculates the utility function and returns it with the policy
#' policy(maze_solved)
#'
#' # Learn a Policy using Q-Learning
#' maze_learned <- solve_MDP(Maze, method = "q_learning", N = 100)
#' maze_learned
#'
#' maze_learned$solution
#' policy(maze_learned)
#' plot_value_function(maze_learned)
#' gridworld_plot_policy(maze_learned)
#' @export
solve_MDP <- function(model, method = "value", ...) {
  methods_DP <- c("value_iteration", "policy_iteration")
  methods_TD <-  c("sarsa", "q_learning", "expected_sarsa")
  
  method <- match.arg(method, c(methods_DP, methods_TD))
  
  if (method %in% methods_DP)
    return(solve_MDP_DP(model, method, ...))
  if (method %in% methods_TD)
    return(solve_MDP_TD(model, method, ...))
  
  # we should not get here!
  stop("Unknown method!")
}

#' @rdname solve_MDP
#' @param U a vector with initial utilities used for each state. If
#'   `NULL`, then the default of a vector of all 0s is used.
#' @param N_max maximum number of iterations allowed to converge. If the
#'   maximum is reached then the non-converged solution is returned with a
#'   warning.
#' @param error value iteration: maximum error allowed in the utility of any state
#'   (i.e., the maximum policy loss) used as the termination criterion.
#' @param k_backups policy iteration: number of look ahead steps used for approximate policy evaluation
#'   used by the policy iteration method.
#' @export
solve_MDP_DP <- function(model,
                         method = "value_iteration",
                         horizon = NULL,
                         discount = NULL,
                         N_max = 1000,
                         error = 0.01,
                         k_backups = 10,
                         U = NULL,
                         verbose = FALSE) {
  if (!inherits(model, "MDP"))
    stop("'model' needs to be of class 'MDP'.")
  
  methods <- c("value_iteration", "policy_iteration")
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
  
  switch(method,
         value_iteration = {
           if (is.infinite(model$horizon))
             MDP_value_iteration_inf_horizon(model,
                                             error,
                                             N_max,
                                             U = U,
                                             verbose = verbose)
           else
             MDP_value_iteration_finite_horizon(model,
                                                horizon = model$horizon,
                                                U = U,
                                                verbose = verbose)
         },
         policy_iteration = {
           if (is.infinite(model$horizon))
             MDP_policy_iteration_inf_horizon(model,
                                              N_max,
                                              k_backups,
                                              U = U,
                                              verbose = verbose)
           else
             stop("Method not implemented yet for finite horizon problems.")
         })
}

# Q-Function:
# the (optimal) state-action value function Q_s(a,k) is the expected total reward
# from stage k onward, if we choose a_k = a and then proceed optimally (given by U).
.QV <-
  function(s, a, P, R, GAMMA, U)
    sum(P[[a]][s,] * (R[[a]][[s]] + GAMMA * U), na.rm = TRUE)
.QV_vec <- Vectorize(.QV, vectorize.args = c("s", "a"))

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
    P <- transition_matrix(model, sparse = TRUE)
    R <-
      reward_matrix(model, sparse = FALSE) ## Note sparse leaves it as a data.frame
    GAMMA <- model$discount
    
    horizon <- as.integer(horizon)
    
    if (is.null(U))
      U <- rep(0, times = length(S))
    names(U) <- S
    
    policy <- vector(mode = "list", length = horizon)
    
    for (t in seq(horizon, 1)) {
      if (verbose)
        cat("Iteration for t = ", t)
      
      Qs <- outer(S, A, .QV_vec, P, R, GAMMA, U)
      m <- apply(Qs, MARGIN = 1, which.max.random)
      
      pi <- factor(m, levels = seq_along(A), labels = A)
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
           error,
           N_max = 1000,
           U = NULL,
           verbose = FALSE) {
    S <- model$states
    A <- model$actions
    P <- transition_matrix(model)
    R <- reward_matrix(model)
    GAMMA <- model$discount
    if (GAMMA < 1)
      convergence_factor <- (1 - GAMMA) / GAMMA
    else
      convergence_factor <- 1
    
    if (is.null(U))
      U <- rep(0, times = length(S))
    names(U) <- S
    
    converged <- FALSE
    for (i in seq_len(N_max)) {
      if (verbose)
        cat("Iteration:", i)
      
      Qs <- outer(S, A, .QV_vec, P, R, GAMMA, U)
      m <- apply(Qs, MARGIN = 1, which.max.random)
      
      pi <- factor(m, levels = seq_along(A), labels = A)
      U_t_minus_1 <- Qs[cbind(seq_along(S), m)]
      
      delta <- max(abs(U_t_minus_1 - U))
      U <- U_t_minus_1
      
      if (verbose)
        cat(" -> delta:", delta, "\n")
      
      if (delta <= error * convergence_factor) {
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
        " Consider decreasing the 'discount' factor or increasing 'error' or 'N_max'."
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

MDP_policy_iteration_inf_horizon <-
  function(model,
           N_max = 1000,
           k_backups = 10,
           U = NULL,
           verbose = FALSE) {
    S <- model$states
    A <- model$actions
    P <- transition_matrix(model, sparse = TRUE)
    R <- reward_matrix(model, sparse = FALSE)
    GAMMA <- model$discount
    
    if (is.null(U)) {
      U <- rep(0, times = length(S))
      pi <- random_MDP_policy(model)$action
    } else {
      # get greedy policy for a given U
      Qs <- outer(S, A, .QV_vec, P, R, GAMMA, U)
      m <- apply(Qs, MARGIN = 1, which.max.random)
      pi <- factor(m, levels = seq_along(A), labels = A)
    }
    
    names(U) <- S
    names(pi) <- S
    
    converged <- FALSE
    for (i in seq_len(N_max)) {
      if (verbose)
        cat("Iteration ", i, "\n")
      
      # evaluate to get U from pi
      U <-
        MDP_policy_evaluation(pi, model, U, k_backups = k_backups)
      
      # get greedy policy for U
      Qs <- outer(S, A, .QV_vec, P, R, GAMMA, U)
      
      # non-randomizes
      m <- apply(Qs, MARGIN = 1, which.max.random)
      pi_prime <- factor(m, levels = seq_along(A), labels = A)
      
      # account for random tie breaking
      #if (all(pi == pi_prime)) {
      if (all(Qs[cbind(seq_len(nrow(Qs)), pi)] == apply(Qs, MARGIN = 1, max))) {
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
        " Consider decreasing the 'discount' factor or increasing 'N_max'."
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

#' @rdname solve_MDP
#' @param alpha step size in `(0, 1]`.
#' @param epsilon used for \eqn{\epsilon}-greedy policies.
#' @param N number of episodes used for learning.
#' @export
solve_MDP_TD <-
  function(model,
           method = "q_learning",
           horizon = NULL,
           discount = NULL,
           alpha = 0.5,
           epsilon = 0.1,
           N = 100,
           U = NULL,
           verbose = FALSE) {
    ### default is infinite horizon, but we use 10000 to guarantee termination
    warn_horizon <- FALSE
    if (is.null(horizon))
      horizon <- model$horizon
    if (is.null(horizon)) {
      if (!any(absorbing_states(model))) 
        stop("The model has no absorbing states. Specify the horizon.")
      warn_horizon <- TRUE
      horizon <- 10000
    }
      
    if (is.null(discount))
      discount <- model$discount
    if (is.null(discount))
      discount <- 1
    gamma <- discount
    model$discount <- discount
    
    S <- model$states
    S_absorbing <- S[which(absorbing_states(model))]
    A <- model$actions
    P <- transition_matrix(model, sparse = TRUE)
    #R <- reward_matrix(model, sparse = FALSE)
    start <- .translate_belief(NULL, model = model)
    
    method <-
      match.arg(method, c("sarsa", "q_learning", "expected_sarsa"))
    
    # Initialize Q
    if (is.null(U))
      Q <-
      matrix(0,
             nrow = length(S),
             ncol = length(A),
             dimnames = list(S, A))
    else
      Q <- q_values_MDP(model, U = U)
    
    # loop episodes
    for (e in seq(N)) {
      s <- sample(S, 1, prob = start)
      a <- greedy_MDP_action(s, Q, epsilon)
      
      # loop steps in episode
      i <- 1L
      while (TRUE) {
        s_prime <- sample(S, 1L, prob = P[[a]][s,])
        r <- reward_matrix(model, a, s, s_prime)
        a_prime <- greedy_MDP_action(s_prime, Q, epsilon)
        
        if (verbose) {
          if (i == 1L)
            cat("\n*** Episode", e, "***\n")
          cat("Step", i, "- s a r s' a':", s, a, r, s_prime, a_prime, "\n")
        }
        
        if (method == "sarsa") {
          # is called Sarsa because it uses the sequence s, a, r, s', a'
          Q[s, a] <-
            Q[s, a] + alpha * (r + gamma * Q[s_prime, a_prime] - Q[s, a])
          if (is.na(Q[s, a]))
            Q[s, a] <- -Inf
        }else if (method == "q_learning") {
          # a' is greedy instead of using the behavior policy
          a_max <- greedy_MDP_action(s_prime, Q, epsilon = 0)
          Q[s, a] <-
            Q[s, a] + alpha * (r + gamma * Q[s_prime, a_max] - Q[s, a])
          if(is.na(Q[s,a]))
            Q[s,a] <- -Inf
        } else if (method == "expected_sarsa") {
          p <-
            greedy_MDP_action(s_prime, Q, epsilon, prob = TRUE)
          exp_Q_prime <-
            sum(greedy_MDP_action(s_prime, Q, epsilon, prob = TRUE) * Q[s_prime,],
                na.rm = TRUE)
          Q[s, a] <-
            Q[s, a] + alpha * (r + gamma * exp_Q_prime - Q[s, a])
          if(is.na(Q[s,a]))
            Q[s,a] <- -Inf
        }
        
        s <- s_prime
        a <- a_prime
        
        if (s %in% S_absorbing)
          break
        
        if (i >= horizon) {
          if (warn_horizon)
            warning(
              "Max episode length of ",
              i,
              " reached without reaching an absorbing state! Terminating episode."
            )
          break
        }
        
        i <- i + 1L
      }
    }
    
    model$solution <- list(
      method = method,
      alpha = alpha,
      epsilon = epsilon,
      N = N,
      Q = Q,
      converged = NA,
      policy = list(data.frame(
        state = S,
        U = apply(Q, MARGIN = 1, max),
        action = A[apply(Q, MARGIN = 1, which.max.random)],
        row.names = NULL
      ))
    )
    
    model
  }
