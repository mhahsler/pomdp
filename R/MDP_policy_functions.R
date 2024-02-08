#' Functions for MDP Policies
#'
#' Implementation several functions useful to deal with MDP policies.
#'
#' Implemented functions are:
#' 
#' * `q_values_MDP()` calculates (approximates) 
#'   Q-values for a given model using the Bellman 
#'   optimality equation: 
#'  
#'   \deqn{q(s,a) = \sum_{s'} T(s'|s,a) [R(s,a) + \gamma U(s')]}
#'
#'   Q-values can be used as the input for several other functions.
#'
#' * `MDP_policy_evaluation()` evaluates a policy \eqn{\pi} for a model and returns 
#'   (approximate) state values by applying the Bellman equation as an update 
#'   rule for each state and iteration \eqn{k}:
#'   
#'   \deqn{U_{k+1}(s) =\sum_a \pi{a|s} \sum_{s'} T(s' | s,a) [R(s,a) + \gamma U_k(s')]}   
#'
#'   In each iteration, all states are updated. Updating is stopped after 
#'   `k_backups` iterations or after the
#'   largest update \eqn{||U_{k+1} - U_k||_\infty < \theta}.
#'   
#' * `greedy_MDP_action()` returns the action with the largest Q-value given a 
#'    state.
#'    
#' * `random_MDP_policy()`, `manual_MDP_policy()`, and `greedy_MDP_policy()`
#'    generates different policies. These policies can be added to a problem
#'    using [`add_policy()`].
#'
#' @name MDP_policy_functions
#' @aliases MDP_policy_functions
#'
#' @family MDP
#' @author Michael Hahsler
#' 
#' @param model an MDP problem specification.
#' @param U a vector with value function representing the state utilities
#'    (expected sum of discounted rewards from that point on).
#'    If `model` is a solved model, then the state
#'    utilities are taken from the solution.
#' @param Q an action value function with Q-values as a state by action matrix.
#' @param s a state.
#' @param epsilon an `epsilon > 0` applies an epsilon-greedy policy.
#'
#' @references
#' Sutton, R. S., Barto, A. G. (2020). Reinforcement Learning: An Introduction. 
#' Second edition. The MIT Press.
#'
#' @examples
#' data(Maze)
#' Maze
#'
#' # create several policies:
#' # 1. optimal policy using value iteration
#' maze_solved <- solve_MDP(Maze, method = "value_iteration")
#' maze_solved
#' pi_opt <- policy(maze_solved)
#' pi_opt
#' gridworld_plot_policy(add_policy(Maze, pi_opt), main = "Optimal Policy")
#'
#' # 2. a manual policy (go up and in some squares to the right)
#' acts <- rep("up", times = length(Maze$states))
#' names(acts) <- Maze$states
#' acts[c("s(1,1)", "s(1,2)", "s(1,3)")] <- "right"
#' pi_manual <- manual_MDP_policy(Maze, acts)
#' pi_manual
#' gridworld_plot_policy(add_policy(Maze, pi_manual), main = "Manual Policy")
#'
#' # 3. a random policy
#' pi_random <- random_MDP_policy(Maze)
#' pi_random
#' gridworld_plot_policy(add_policy(Maze, pi_random), main = "Random Policy")
#'
#' # 4. an improved policy based on one policy evaluation and
#' #   policy improvement step
#' u <- MDP_policy_evaluation(pi_random, Maze, verbose = TRUE)
#' q <- q_values_MDP(Maze, U = u)
#' pi_greedy <- greedy_MDP_policy(q)
#' pi_greedy
#' gridworld_plot_policy(add_policy(Maze, pi_greedy), , main = "Greedy Policy")
#'
#' #' compare the approx. value functions for the policies
#' rbind(
#'   random = MDP_policy_evaluation(pi_random, Maze),
#'   manual = MDP_policy_evaluation(pi_manual, Maze),
#'   greedy = MDP_policy_evaluation(pi_greedy, Maze),
#'   optimal = MDP_policy_evaluation(pi_opt, Maze)
#')
#'
#' # For many functions, we first add the policy to the problem description
#' #   to create a "solved" MDP
#' maze_random <- add_policy(Maze, pi_random)
#' maze_random
#'
#' # plotting
#' plot_value_function(maze_random)
#' gridworld_plot_policy(maze_random)
#' 
#' # compare to a benchmark
#' regret(maze_random, benchmark = maze_solved)
#'
#' # calculate greedy actions for state 1
#' q <- q_values_MDP(maze_random)
#' q
#' greedy_MDP_action(1, q, epsilon = 0, prob = FALSE)
#' greedy_MDP_action(1, q, epsilon = 0, prob = TRUE)
#' greedy_MDP_action(1, q, epsilon = .1, prob = TRUE)
NULL

#' @rdname MDP_policy_functions
#' @return `q_values_MDP()` returns a state by action matrix specifying the Q-function,
#'   i.e., the action value for executing each action in each state. The Q-values
#'   are calculated from the value function (U) and the transition model.
#' @export
q_values_MDP <- function(model, U = NULL) {
  if (!inherits(model, "MDP"))
    stop("'model' needs to be of class 'MDP'.")
  
  S <- model$states
  A <- model$actions
  P <- transition_matrix(model, sparse = TRUE)
  # TODO: sparse = TRUE returns a dataframe. This uses a lot of memory!
  R <- reward_matrix(model, sparse = FALSE)
  policy <- model$solution$policy[[1]]
  GAMMA <- model$discount
  
  ### TODO: look if Q is already stored in the model!
  if (is.null(U)) {
    if (!is.null(policy))
      U <- policy$U
    else
      stop("'model' does not contain state utilities (it is unsolved). You need to specify U.")
  }
  
  structure(outer(S, A, .QV_vec, P, R, GAMMA, U), dimnames = list(S, A))
}


#' @rdname MDP_policy_functions
#' @param pi a policy as a data.frame with at least columns for states and action.
#' @param k_backups number of look ahead steps used for approximate policy evaluation
#'    used by the policy iteration method. Set k_backups to `Inf` to only use 
#'    \eqn{\theta} as the stopping criterion.
#' @param theta stop when the largest change in a state value is less 
#'    than \eqn{\theta}. 
#' @param verbose logical; should progress and approximation errors be printed.
#' @return `MDP_policy_evaluation()` returns a vector with (approximate)
#'    state values (U).
#' @export
MDP_policy_evaluation <-
  function(pi,
           model,
           U = NULL,
           k_backups = 1000,
           theta = 1e-3,
           verbose = FALSE) {
    if (!inherits(model, "MDP"))
      stop("'model' needs to be of class 'MDP'.")
    
    S <- model$states
    A <- model$actions
    P <- transition_matrix(model, sparse = TRUE)
    R <- reward_matrix(model, sparse = FALSE)
    GAMMA <- model$discount
    
    if (is.data.frame(pi))
      pi <- pi$action
    names(pi) <- S
    
    # start with all 0s if no previous U is given
    if (is.null(U))
      U <- rep(0, times = length(S))
    
    if (k_backups > .Machine$integer.max)
      k_backups <- .Machine$integer.max
    for (i in seq_len(k_backups)) {
      v <- U
      U <- .QV_vec(S, pi, P, R, GAMMA, U)
      delta <- max(abs(v-U))
      
      if (verbose)
        cat("Backup step", i, ": delta =", delta, "\n")
      
      if (delta < theta)
        break
    }
    
    U
  }

#' @rdname MDP_policy_functions
#' @return `greedy_MDP_action()` returns the action with the highest q-value
#'    for state `s`. If `prob = TRUE`, then a vector with
#'    the probability for each action is returned.
#' @export
greedy_MDP_action <-
  function(s,
           Q,
           epsilon = 0,
           prob = FALSE) {
    available_A <- colnames(Q)
    
    if (!prob) {
      if (epsilon == 0 ||
          length(available_A) == 1L || runif(1) > epsilon) {
        a <- available_A[which.max.random(Q[s, available_A])]
      } else {
        a <- sample(available_A, size = 1L)
      }
      
      return (a)
    }
    
    #return probabilities
    p <- structure(rep(0, ncol(Q)), names = colnames(Q))
    a <- available_A[which.max.random(Q[s, available_A])]
    p[a] <- 1 - epsilon
    p[available_A] <- p[available_A] + epsilon / length(available_A)
    
    return(p)
  }


#' @rdname MDP_policy_functions
#' @param prob probability vector for random actions for `random_MDP_policy()`.
#'   a logical indicating if action probabilities should be returned for
#'   `greedy_MDP_action()`.
#' @return `random_MDP_policy()` returns a data.frame with the columns state and action to define a policy.
#' @export
random_MDP_policy <-
  function(model, prob = NULL) {
    if (!inherits(model, "MDP"))
      stop("'model' needs to be of class 'MDP'.")
    
    A <- model$actions
    S <- model$states
    
    data.frame(state = S,
               action = factor(
                 sample(
                   seq_along(A),
                   size = length(S),
                   replace = TRUE,
                   prob = prob
                 ),
                 levels = seq_along(A),
                 labels = A
               ))
  }

#' @rdname MDP_policy_functions
#' @param actions a vector with the action (either the action label or the
#'  numeric id) for each state.
#' @return `manual_MDP_policy()` returns a data.frame with the columns state and action to define a policy.
#' @export
manual_MDP_policy <-
  function(model, actions) {
    if (!inherits(model, "MDP"))
      stop("'model' needs to be of class 'MDP'.")
    
    A <- model$actions
    S <- model$states
    
    if (is.numeric(actions))
      actions <- A[actions]
    
    actions <- factor(actions, levels = A)
    
    data.frame(state = S,
               action = actions)
  }

#' @rdname MDP_policy_functions
#' @return `greedy_MDP_policy()` returns the greedy policy given `Q`.
#' @export
greedy_MDP_policy <-
  function(Q) {
    A <- colnames(Q)
    data.frame(
      state = rownames(Q),
      U = apply(Q, MARGIN = 1, max),
      action = A[apply(Q, MARGIN = 1, which.max.random)],
      row.names = NULL
    )
  }


