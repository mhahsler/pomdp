#' Simulate Trajectories in a MDP
#'
#' Simulate trajectories through a MDP. The start state for each
#' trajectory is randomly chosen using the specified belief. The belief is used to choose actions
#' from an epsilon-greedy policy and then update the state.
#'
#' A native R implementation is available (`engine = 'r'`) and the default is a
#' faster C++ implementation (`engine = 'cpp'`).
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
#' @param horizon epochs end once an absorbing state is reached or after 
#'  the maximal number of epochs specified via `horizon`. If `NULL` then the
#'  horizon for the model is used.
#' @param epsilon the probability of random actions  for using an epsilon-greedy policy.
#'  Default for solved models is 0 and for unsolved model 1.
#' @param engine `'cpp'` or `'r'` to perform simulation using a faster C++
#'  or a native R implementation.
#' @param return_trajectories logical; return the complete trajectories.
#' @param delta_horizon precision used to determine the horizon for infinite-horizon problems.
#' @param verbose report used parameters.
#' @param ... further arguments are ignored.
#' @return A list with elements:
#'  * `avg_reward`: The average discounted reward.
#'  * `reward`: Reward for each trajectory.
#'  * `action_cnt`: Action counts.
#'  * `state_cnt`: State counts.
#'  * `trajectories`: A data.frame with the trajectories. Each row 
#'    contains the `episode` id, the `time` step, the state `s`, 
#'    the chosen action `a`,
#'    the reward `r`, and the next state `s_prime`. Trajectories are 
#'    only returend of `return_trajectories = TRUE`.
#' @author Michael Hahsler
#' @examples
#' # enable parallel simulation 
#' # doParallel::registerDoParallel()
#' 
#' data(Maze)
#'
#' # solve the POMDP for 5 epochs and no discounting
#' sol <- solve_MDP(Maze, discount = 1)
#' sol
#'
#' # U in the policy is and estimate of the utility of being in a state when using the optimal policy.
#' policy(sol)
#' gridworld_matrix(sol, what = "action")
#'
#' ## Example 1: simulate 100 trajectories following the policy, 
#' #             only the final belief state is returned
#' sim <- simulate_MDP(sol, n = 100, horizon = 10, verbose = TRUE)
#' sim
#'
#' # Note that all simulations start at s_1 and that the simulated avg. reward
#' # is therefore an estimate to the U value for the start state s_1.
#' policy(sol)[1,]
#'
#' # Calculate proportion of actions taken in the simulation
#' round_stochastic(sim$action_cnt / sum(sim$action_cnt), 2)
#'
#' # reward distribution
#' hist(sim$reward)
#'
#' ## Example 2: simulate starting following a uniform distribution over all
#' #             states and return all trajectories
#' sim <- simulate_MDP(sol, n = 100, start = "uniform", horizon = 10, 
#'   return_trajectories = TRUE)
#' head(sim$trajectories)   
#'   
#' # how often was each state visited?
#' table(sim$trajectories$s)
#' @export
simulate_MDP <-
  function(model,
           n = 100,
           start = NULL,
           horizon = NULL,
           epsilon = NULL,
           delta_horizon = 1e-3,
           return_trajectories = FALSE,
           engine = "cpp",
           verbose = FALSE,
           ...) {
    engine <- match.arg(tolower(engine), c("cpp", "r"))
    
    start <- .translate_belief(start, model = model)
    solved <- is_solved_MDP(model)
    
    n <- as.integer(n)
    
    if (is.null(horizon))
      horizon <- model$horizon
    if (is.null(horizon) || is.infinite(horizon)) {
      if (is.null(model$discount) || !(model$discount < 1))
        stop("Simulation needs a finite simulation horizon.")
      
      # find a horizon that approximates the reward using
      # discount^horizon * max_abs_R <= 0.001
      max_abs_R <-
        max(abs(reward_matrix(model, sparse = TRUE)$value))
      horizon <-
        ceiling(log(delta_horizon / max_abs_R) / log(model$discount))
    }
    horizon <- as.integer(horizon)
    
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
    
    if (engine == "cpp") {
      ### TODO: Make sparse
      model <- normalize_MDP(model, sparse = TRUE)
      
      if (foreach::getDoParWorkers() == 1 || n * horizon < 100000)
        return (simulate_MDP_cpp(
          model,
          n,
          start,
          horizon,
          disc,
          return_trajectories,
          epsilon,
          verbose = verbose
        ))
      
      ns <- foreach_split(n)
      
      if (verbose) {
        cat("Simulating MDP trajectories.\n")
        cat("- engine: cpp \n")
        cat("- horizon:", horizon, "\n")
        cat("- n:", n, "- parallel workers:", length(ns), "\n")
        cat("- epsilon:", epsilon, "\n")
        cat("- discount factor:", disc, "\n")
        cat("\n")
      }
      
      w <-
        NULL # to shut up the warning for the foreach counter variable
      
      sim <- foreach(w = 1:length(ns)) %dopar%
        simulate_MDP_cpp(model,
                         ns[w],
                         start,
                         horizon,
                         disc,
                         return_trajectories,
                         epsilon,
                         verbose = FALSE)
      
      # adjust the episode number for parallel processing
      episode_add <- cumsum(c(0L, ns))
      for (i in seq_along(sim)) {
        sim[[i]]$trajectories$episode <-
          sim[[i]]$trajectories$episode + episode_add[i]
      }
      
      rew <- Reduce(c,  lapply(sim, "[[", "reward"))
     
      return(
        list(
          avg_reward = mean(rew, na.rm = TRUE),
          reward = rew,
          action_cnt = Reduce('+', lapply(sim, "[[" , "action_cnt")),
          state_cnt =  Reduce('+', lapply(sim, "[[", "state_cnt")),
          trajectories = Reduce(rbind, lapply(sim, "[[", "trajectories"))
        )
      )
      
    }
    
    ######### R implementation starts here ##############
    
    states <- as.character(model$states)
    n_states <- length(states)
    states_absorbing <- which(absorbing_states(model))
    actions <- as.character(model$actions)
    
    trans_m <- transition_matrix(model, sparse = NULL)
    #rew_m <- reward_matrix(model, sparse = NULL)
    
    # for easier access
    pol <-
      lapply(
        model$solution$policy,
        FUN = function(p)
          structure(p$action, names = p$state)
      )
    
    if (verbose) {
      cat("Simulating MDP trajectories.\n")
      cat("- engine:", engine, "\n")
      cat("- horizon:", horizon, "\n")
      cat("- n:",
          n,
          "- parallel workers:",
          foreach::getDoParWorkers(),
          "\n")
      cat("- epsilon:", epsilon, "\n")
      cat("- discount factor:", disc, "\n")
      cat("\n")
    }
    
    #warning("Debug mode on!!!")
    #sim <- replicate(n, expr = {
    sim <- foreach(i = 1:n) %dopar% {
      # find a initial state
      s <- sample.int(length(states), 1L, prob = start)
      
      action_cnt <- rep(0L, length(actions))
      names(action_cnt) <- actions
      state_cnt <- rep(0L, length(states))
      names(state_cnt) <- states
      rew <- 0
      
      if (return_trajectories)
        trajectory <- data.frame(
          episode = rep(NA_integer_, horizon),
          time = rep(NA_integer_, horizon),
          s = NA_integer_,
          a = NA_integer_,
          r = NA_real_,
          s_prime = NA_integer_
        )
      else
        trajectory <- NULL
      
      for (j in seq_len(horizon)) {
        if (runif(1) < epsilon) {
          a <- sample.int(length(actions), 1L, replace = TRUE)
        } else {
          a <- pol[[.get_pol_index(model, j)]][s]
        }
        
        action_cnt[a] <- action_cnt[a] + 1L
        state_cnt[s] <- state_cnt[s] + 1L
        
        s_prev <- s
        s <-
          sample.int(length(states), 1L, prob = trans_m[[a]][s,])
        
        #rew <- rew + rew_m[[a]][[s_prev]][s] * disc ^ (j - 1L)
        # MDPs have no observation!
        r <- reward_matrix(model, a, s_prev, s)
        rew <- rew + r * disc ^ (j - 1L)
        
        if (return_trajectories)
          trajectory[j, ] <-
          data.frame(
            episode = i,
            time = j - 1L,
            s = s_prev,
            a = a,
            r = r,
            s_prime = s
          )
      
        if(s %in% states_absorbing) {
          if (return_trajectories)
            trajectory <- trajectory[1:j, , drop = FALSE]
            # TODO: maybe add the finals state
          break
        }
        
      }
      
      list(
        action_cnt =  action_cnt,
        state_cnt = state_cnt,
        reward = rew,
        trajectory = trajectory
      )
    }
    
    rew <- Reduce(c, lapply(sim, "[[", "reward"))
    rew <- unname(rew)
    
    trajectories <- NULL
    if (return_trajectories) {
      trajectories <- Reduce(rbind, lapply(sim, "[[", "trajectory"))
      trajectories$s <-
        factor(trajectories$s,
               levels = seq_along(states),
               labels = states)
      trajectories$a <-
        factor(trajectories$a,
               levels = seq_along(actions),
               labels = actions)
      trajectories$s_prime <-
        factor(trajectories$s_prime,
               levels = seq_along(states),
               labels = states)
    }
    
    list(
      avg_reward = mean(rew, na.rm = TRUE),
      reward = rew,
      action_cnt = Reduce('+', lapply(sim, "[[", "action_cnt")),
      state_cnt = Reduce('+', lapply(sim, "[[", "state_cnt")),
      trajectories = trajectories
    )
  }
