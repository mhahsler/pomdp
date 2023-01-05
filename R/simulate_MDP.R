#' Simulate Trajectories in a MDP
#'
#' Simulate trajectories through a MDP. The start state for each
#' trajectory is randomly chosen using the specified belief. The belief is used to choose actions
#' from an epsilon-greedy policy and then update the state.
#'
#' A native R implementation is available (`method = 'r'`) and the default is a 
#' faster C++ implementation (`method = 'cpp'`). 
#' 
#' Both implementations support parallel execution using the package
#' \pkg{foreach}. To enable parallel execution, a parallel backend like
#' \pkg{doparallel} needs to be available needs to be registered (see
#' [doParallel::registerDoParallel()]). 
#' Note that small simulations are slower using parallelization. Therefore, C++ simulations 
#' with n * horizon less than 100,000 are always executed using a single worker.
#' @family MDP
#' @importFrom stats runif
#'
#' @param model a MDP model.
#' @param n number of trajectories.
#' @param start probability distribution over the states for choosing the
#'  starting states for the trajectories. Defaults to "uniform".
#' @param horizon number of epochs for the simulation. If `NULL` then the
#'  horizon for the model is used.
#' @param return_states logical; return visited states.
#' @param epsilon the probability of random actions  for using an epsilon-greedy policy.
#'  Default for solved models is 0 and for unsolved model 1.
#' @param method `'cpp'` or `'r'` to perform simulation using a faster C++ 
#'  or a native R implementation. 
#' @param verbose report used parameters.
#' @return A list with elements:
#'  * `avg_reward`: The average discounted reward.
#'  * `reward`: Reward for each trajectory.
#'  * `action_cnt`: Action counts.
#'  * `state_cnt`: State counts.
#'  * `states`: a vector with state ids.
#'    Rows represent trajectories.
#' @author Michael Hahsler
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
#' # U in the policy is and estimate of the utility of being in a state when using the optimal policy.
#'
#' ## Example 1: simulate 10 trajectories, only the final belief state is returned
#' sim <- simulate_MDP(sol, n = 100, horizon = 10, verbose = TRUE)
#' sim
#'
#' # Calculate proportion of actions used
#' round_stochastic(sim$action_cnt / sum(sim$action_cnt), 2)
#'
#' # reward distribution
#' hist(sim$reward)
#'
#' ## Example 2: simulate starting always in state s_1 and return all visited states
#' sim <- simulate_MDP(sol, n = 100, start = "s_1", horizon = 10, return_states = TRUE)
#' sim$avg_reward
#'
#' # how often was each state visited?
#' table(sim$states)
#' @export
simulate_MDP <-
  function(model,
    n = 100,
    start = NULL,
    horizon = NULL,
    return_states = FALSE,
    epsilon = NULL,
    method = "cpp",
    verbose = FALSE) {
    method <- match.arg(tolower(method), c("r", "cpp"))
    
    start <- .translate_belief(start, model = model)
    solved <- .solved_MDP(model)
    
    if (is.null(horizon))
      horizon <- model$horizon
    if (is.null(horizon))
      stop("The horizon (number of epochs) has to be specified!")
    if (is.infinite(horizon))
      stop("Simulation needs a finite simulation horizon.")
    
    if (is.null(epsilon)) {
      if (!solved)
        epsilon <- 1
      else
        epsilon <- 0
    }
    
    if (!solved && epsilon != 1)
      stop("epsilon has to be 1 for unsolved models.")
    
    disc <- model$discount
    if (is.null(disc))
      disc <- 1
    
    if (method == "cpp") {
      model <- normalize_MDP(model)
      
      if (foreach::getDoParWorkers() == 1 || n * horizon < 100000)
        return (simulate_MDP_cpp(
          model,
          n,
          start,
          horizon,
          disc,
          return_states,
          epsilon,
          verbose
        ))
      
      nw <- foreach::getDoParWorkers()
      ns <- rep(ceiling(n / nw), times = nw)
      dif <- sum(ns) - n
      if (dif > 0)
        ns[1:dif] <- ns[1:dif] - 1
      
      if (verbose) {
        cat("Simulating MDP trajectories.\n")
        cat("- method: cpp \n")
        cat("- horizon:", horizon, "\n")
        cat("- n:", n, "- parallel workers:", foreach::getDoParWorkers(), "\n")
        cat("- epsilon:", epsilon, "\n")
        cat("- discount factor:", disc, "\n")
        cat("\n")
      }
      
      w <-
        NULL # to shut up the warning for the foreach counter variable
      
      sim <- foreach(w = 1:nw) %dopar%
        simulate_MDP_cpp(model,
          ns[w],
          start,
          horizon,
          disc,
          return_states,
          epsilon,
          verbose = FALSE)
      
      rew <- Reduce(c,  lapply(sim, "[[", "reward"))
      
      return(
        list(
          avg_reward = mean(rew, na.rm = TRUE),
          reward = rew,
          action_cnt = Reduce('+', lapply(sim, "[[" , "action_cnt")),
          state_cnt =  Reduce('+', lapply(sim, "[[", "state_cnt")),
          states = Reduce(c, lapply(sim, "[[", "states"))
        )
      )
      
    }
    
    ## R implementation starts here
    states <- as.character(model$states)
    n_states <- length(states)
    actions <- as.character(model$actions)
    
    trans_m <- transition_matrix(model, sparse = TRUE)
    rew_m <- reward_matrix(model, sparse = TRUE)
    
    # for easier access
    pol <-
      lapply(
        model$solution$policy,
        FUN = function(p)
          structure(p$action, names = p$state)
      )
    
    if (verbose) {
      cat("Simulating MDP trajectories.\n")
      cat("- method:", method, "\n")
      cat("- horizon:", horizon, "\n")
      cat("- n:", n, "- parallel workers:", foreach::getDoParWorkers(), "\n")
      cat("- epsilon:", epsilon, "\n")
      cat("- discount factor:", disc, "\n")
      cat("\n")
    }
    
    #st <- replicate(n, expr = {
    sim <- times(n) %dopar% {
      # find a initial state
      s <- sample(states, 1, prob = start)
      
      action_cnt <- rep(0L, length(actions))
      names(action_cnt) <- actions
      state_cnt <- rep(0L, length(states))
      names(state_cnt) <- states
      rew <- 0
      
      if (return_states)
        states_visited <- integer(horizon)
      else
        states_visited <- integer()
      
      for (j in 1:horizon) {
        if (runif(1) < epsilon) {
          a <- sample.int(length(actions), 1L, replace = TRUE)
        } else {
          a <- pol[[.get_pol_index(model, j)]][s]
        }
        
        action_cnt[a] <- action_cnt[a] + 1L
        state_cnt[s] <- state_cnt[s] + 1L
        
        s_prev <- s
        s <-
          sample.int(length(states), 1L, prob = trans_m[[a]][s, ])
        
        rew <- rew + rew_m[[a]][[s_prev]][s] * disc ^ (j - 1L)
        
        if (return_states)
          states_visited[j] <- s
      }
      
      states_visited <-
        factor(states_visited,
          levels = seq_along(model$states),
          labels = model$states)
      
      list(list(
        action_cnt =  action_cnt,
        state_cnt = state_cnt,
        reward = rew,
        states = states_visited
      ))
    }
    #, simplify = FALSE)
    
    rew <- Reduce(c, lapply(sim, "[[", "reward"))
    rew <- unname(rew)
    
    list(
      avg_reward = mean(rew, na.rm = TRUE),
      reward = rew,
      action_cnt = Reduce('+', lapply(sim, "[[", "action_cnt")),
      state_cnt = Reduce('+', lapply(sim, "[[", "state_cnt")),
      states = Reduce(c, lapply(sim, "[[", "states"))
    )
  }
