#' Functions for MDP Policies
#'
#' Implementation several functions useful to deal with MDP policies.
#'
#' @name MDP_policy_functions
#' @aliases MDP_policy_functions
#'
#' @family MDP
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
#' plot_Maze_solution(add_policy(Maze, pi_opt), main = "Optimal Policy")
#'
#' # 2. a manual policy (go up and in some squares to the right)
#' acts <- rep("up", times = length(Maze$states))
#' acts[c(3, 6, 9)] <- "right"
#' pi_manual <- manual_MDP_policy(Maze, acts)
#' pi_manual
#' plot_Maze_solution(add_policy(Maze, pi_manual), main = "Manual Policy")
#'
#' # 3. a random policy
#' pi_random <- random_MDP_policy(Maze)
#' pi_random
#' plot_Maze_solution(add_policy(Maze, pi_random), main = "Random Policy")
#'
#' # 4. an improved policy based on one policy evaluation and
#' #   policy improvement step
#' u <- approx_MDP_policy_evaluation(pi_random, Maze)
#' q <- q_values_MDP(Maze, U = u)
#' pi_greedy <- greedy_MDP_policy(q)
#' pi_greedy
#' plot_Maze_solution(add_policy(Maze, pi_greedy), , main = "Greedy Policy")
#'
#' #' compare the approx. value functions for the policies
#' rbind(
#'   random = approx_MDP_policy_evaluation(pi_random, Maze),
#'   manual = approx_MDP_policy_evaluation(pi_manual, Maze),
#'   greedy = approx_MDP_policy_evaluation(pi_greedy, Maze),
#'   optimal = approx_MDP_policy_evaluation(pi_opt, Maze)
#')
#'
#' # For many functions, we first add the policy to the problem description
#' #   to create a "solved" MDP
#' maze_random <- add_policy(Maze, pi_random)
#' maze_random
#'
#' # plotting
#' plot_value_function(maze_random)
#' plot_Maze_solution(maze_random)
#' 
#' # compare to a benchmark
#' regret(maze_random, maze_solved, start = "s_1")
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
  # FIXME: sparse = TRUE returns a dataframe
  R <- reward_matrix(model, sparse = FALSE)
  policy <- model$solution$policy[[1]]
  GAMMA <- model$discount
  
  ### TODO: look for Q!
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
#'    used by the policy iteration method.
#' @return `approx_MDP_policy_evaluation()` returns a vector with approximate
#'    state values (U).
#' @export
approx_MDP_policy_evaluation <-
  function(pi,
           model,
           U = NULL,
           k_backups = 10) {
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
    
    for (i in seq_len(k_backups))
      U <- .QV_vec(S, pi, P, R, GAMMA, U)
    
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
        a <- available_A[which.max(Q[s, available_A])]
      } else {
        a <- sample(available_A, size = 1L)
      }
      
      return (a)
    }
    
    #return probabilities
    p <- structure(rep(0, ncol(Q)), names = colnames(Q))
    a <- available_A[which.max(Q[s, available_A])]
    p[a] <- 1 - epsilon
    p[available_A] <- p[available_A] + epsilon / length(available_A)
    
    return(p)
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
      action = A[apply(Q, MARGIN = 1, which.max)],
      row.names = NULL
    )
  }


#' @rdname MDP_policy_functions
#' @param prob probability vector for random actions for random_MDP_policy().
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
