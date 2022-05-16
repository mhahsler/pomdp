## TODO: Reimplement in C++

#' Simulate Trajectories in a MDP
#'
#' Simulate trajectories through a MDP. The start state for each
#' trajectory is randomly chosen using the specified belief. The belief is used to choose actions
#' from an epsilon-greedy policy and then update the state.
#'
#' @family MDP
#' @importFrom stats runif
#'
#' @param model a MDP model.
#' @param n number of trajectories.
#' @param start probability distribution over the states for choosing the
#' starting states for the trajectories.
#' Defaults to "uniform".
#' @param horizon number of epochs for the simulation. If `NULL` then the
#' horizon for the model is used.
#' @param visited_states logical; Should all visited states on the
#' trajectories be returned? If `FALSE` then only the final
#' state is returned.
#' @param epsilon the probability of random actions  for using an epsilon-greedy policy.
#' Default for solved models is 0 and for unsolved model 1.
#' @param verbose report used parameters.
#' @return A vector with state ids (in the final epoch or all). Attributes containing action
#' counts, and rewards  for each trajectory may be available.
#' @author Michael Hahsler
#' @md
#' @examples
#' data(Maze)
#'
#' # solve the POMDP for 5 epochs and no discounting
#' sol <- solve_MDP(Maze, discount = 1)
#' sol
#' policy(sol)
#'
#' ## Example 1: simulate 10 trajectories, only the final belief state is returned
#' sim <- simulate_MDP(sol, n = 10, horizon = 10, verbose = TRUE)
#' head(sim)
#'
#' # additional data is available as attributes
#' names(attributes(sim))
#' attr(sim, "avg_reward")
#' colMeans(attr(sim, "action"))
#' 
#' ## Example 2: simulate starting always in state s_1
#' sim <- simulate_MDP(sol, n = 100, start = "s_1", horizon = 10)
#' sim
#' 
#' # the average reward is an estimate of the utility in the optimal policy:
#' policy(sol)[[1]][1,]
#' 
#' @export
simulate_MDP <-
  function(model,
    n = 100,
    start = NULL,
    horizon = NULL,
    visited_states = FALSE,
    epsilon = NULL,
    verbose = FALSE) {
    
    start <- .translate_belief(start, model = model)
    solved <- .solved_MDP(model)
    
    if (is.null(horizon))
      horizon <- model$horizon
    if (is.null(horizon))
      stop("The horizon (number of epochs) has to be specified!")
    if (is.infinite(horizon))
      stop("Simulation needs a finite simulation horizon.")
    
    if (is.null(epsilon)) {
      if (!solved) epsilon <- 1
      else epsilon <- 0
    }
      
    if (!solved && epsilon != 1)
      stop("epsilon has to be 1 for unsolved models.")
    
    disc <- model$discount
    if (is.null(disc))
      disc <- 1
    
    states <- as.character(model$states)
    n_states <- length(states)
    actions <- as.character(model$actions)
    
    trans_m <- transition_matrix(model)
    rew_m <- reward_matrix(model)
    
    # for easier access
    pol <- lapply(model$solution$policy, FUN = function(p) structure(p$action, names = p$state))
    
    if (verbose) {
      cat("Simulating MDP trajectories.\n")
      cat("- horizon:", horizon, "\n")
      cat("- epsilon:", epsilon, "\n")
      cat("- discount factor:", disc, "\n")
      cat("- starting state:\n")
      print(start)
      cat("\n")
    }
    
    st <- replicate(n, expr = {
      # find a initial state
      
      s <- sample(states, 1, prob = start)
      
      action_cnt <- rep(0L, length(actions))
      names(action_cnt) <- actions
      state_cnt <- rep(0L, length(states))
      names(state_cnt) <- states
      
      rew <- 0
      
      if (visited_states)
        s_all <- integer(horizon)
      
      for (j in 1:horizon) {
        
        if (runif(1) < epsilon) {
          a <- sample.int(length(actions), 1L, replace = TRUE)
        } else {
          a <- pol[[.get_pol_index(model, j)]][s]
        }
        
        action_cnt[a] <- action_cnt[a] + 1L
        state_cnt[s] <- state_cnt[s] + 1L
        
        s_prev <- s
        s <- sample.int(length(states), 1L, prob = trans_m[[a]][s,])
        
        rew <- rew + rew_m[[a]][[s_prev]][s] * disc ^ (j - 1L)
        
        if (visited_states)
          s_all[j] <- s
      }
      
      if (!visited_states)
        s_all <- s
      
      rownames(s_all) <- NULL
      attr(s_all, "action_cnt") <- action_cnt
      attr(s_all, "state_cnt") <- state_cnt
      attr(s_all, "reward") <- rew
      s_all
      
    }, simplify = FALSE)
    
    ac <- do.call(rbind, lapply(st, attr, "action_cnt"))
    rownames(ac) <- NULL
    sc <- do.call(rbind, lapply(st, attr, "state_cnt"))
    rownames(sc) <- NULL
    rew <- do.call(rbind, lapply(st, attr, "reward"))
    rownames(rew) <- NULL
    st <- do.call(c, st)
    
    attr(st, "action_cnt") <- ac
    attr(st, "state_cnt") <- sc
    attr(st, "reward") <- rew
    attr(st, "avg_reward") <- mean(rew, na.rm = TRUE)
    
    st
  }
