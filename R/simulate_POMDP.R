#' Simulate Trajectories Through a POMDP
#'
#' Simulate trajectories through a POMDP. The start state for each
#' trajectory is randomly chosen using the specified belief. The belief is used to choose actions
#' from the the epsilon-greedy policy and then updated using observations.
#'
#' Simulates `n` trajectories.
#' If no simulation horizon is specified, the horizon of finite-horizon problems 
#' is used. For infinite-horizon problems with \eqn{\gamma < 1}, the simulation
#' horizon \eqn{T} is chosen such that 
#' the worst-case error is no more than \eqn{\delta_\text{horizon}}. That is
#' 
#' \deqn{\gamma^T \frac{R_\text{max}}{\gamma} \le \delta_\text{horizon},}
#' 
#' where \eqn{R_\text{max}} is the largest possible absolute reward value used as a
#' perpetuity starting after \eqn{T}.
#' 
#' A native R implementation (`engine = 'r'`) and a faster C++ implementation
#' (`engine = 'cpp'`) are available. Currently, only the R implementation supports 
#' multi-episode problems.
#' 
#' Both implementations support the simulation of trajectories in parallel using the package
#' \pkg{foreach}. To enable parallel execution, a parallel backend like
#' \pkg{doparallel} needs to be registered (see
#' [doParallel::registerDoParallel()]). 
#' Note that small simulations are slower using parallelization. C++ simulations 
#' with `n * horizon` less than 100,000 are always executed using a single worker.
#'
#' @family POMDP
#' @importFrom stats runif
#'
#' @param model a POMDP model.
#' @param n number of trajectories.
#' @param belief probability distribution over the states for choosing the
#'  starting states for the trajectories.
#'  Defaults to the start belief state specified in the model or "uniform".
#' @param horizon number of epochs for the simulation. If `NULL` then the
#'  horizon for finite-horizon model is used. For infinite-horizon problems, a horizon is 
#'  calculated using the discount factor.
#' @param epsilon the probability of random actions for using an epsilon-greedy policy.
#'  Default for solved models is 0 and for unsolved model 1.
#' @param delta_horizon precision used to determine the horizon for infinite-horizon problems.
#' @param digits round probabilities for belief points.
#' @param return_beliefs logical; Return all visited belief states? This requires n x horizon memory.
#' @param return_trajectories logical; Return the simulated trajectories as a data.frame?
#' @param engine `'cpp'`, `'r'` to perform simulation using a faster C++ or a 
#'  native R implementation.
#' @param verbose report used parameters.
#' @param ... further arguments are ignored.
#' @return A list with elements:
#'  * `avg_reward`: The average discounted reward.
#'  * `action_cnt`: Action counts.
#'  * `state_cnt`: State counts.
#'  * `reward`: Reward for each trajectory.
#'  * `belief_states`: A matrix with belief states as rows.
#'  * `trajectories`: A data.frame with the `episode` id, `time`, the state of the 
#'    simulation (`simulation_state`), the id of the used alpha vector given the current belief 
#'    (see `belief_states` above), the action `a` and the reward `r`.
#' @author Michael Hahsler
#' @md
#' @examples
#' data(Tiger)
#'
#' # solve the POMDP for 5 epochs and no discounting
#' sol <- solve_POMDP(Tiger, horizon = 5, discount = 1, method = "enum")
#' sol
#' policy(sol)
#'
#' # uncomment the following line to register a parallel backend for simulation 
#' # (needs package doparallel installed)
#' 
#' # doParallel::registerDoParallel()
#' # foreach::getDoParWorkers()
#'
#' ## Example 1: simulate 100 trajectories
#' sim <- simulate_POMDP(sol, n = 100, verbose = TRUE)
#' sim
#'
#' # calculate the percentage that each action is used in the simulation
#' round_stochastic(sim$action_cnt / sum(sim$action_cnt), 2)
#'
#' # reward distribution
#' hist(sim$reward)
#'
#' 
#' ## Example 2: look at the belief states and the trajectories starting with 
#' #             an initial start belief.
#' sim <- simulate_POMDP(sol, n = 100, belief = c(.5, .5), 
#'   return_beliefs = TRUE, return_trajectories = TRUE)
#' head(sim$belief_states)
#' head(sim$trajectories)
#'
#' # plot with added density (the x-axis is the probability of the second belief state)
#' plot_belief_space(sol, sample = sim$belief_states, jitter = 2, ylim = c(0, 6))
#' lines(density(sim$belief_states[, 2], bw = .02)); axis(2); title(ylab = "Density")
#'
#'
#' ## Example 3: simulate trajectories for an unsolved POMDP which uses an epsilon of 1
#' #             (i.e., all actions are randomized). The simulation horizon for the 
#' #             infinite-horizon Tiger problem is calculated using delta_horizon. 
#' sim <- simulate_POMDP(Tiger, return_beliefs = TRUE, verbose = TRUE)
#' sim$avg_reward
#' 
#' hist(sim$reward, breaks = 20)
#' 
#' plot_belief_space(sol, sample = sim$belief_states, jitter = 2, ylim = c(0, 6))
#' lines(density(sim$belief_states[, 1], bw = .05)); axis(2); title(ylab = "Density")
#' @export
simulate_POMDP <-
  function(model,
    n = 1000,
    belief = NULL,
    horizon = NULL,
    epsilon = NULL,
    delta_horizon = 1e-3,
    digits = 7L,
    return_beliefs = FALSE,
    return_trajectories = FALSE,
    engine = "cpp",
    verbose = FALSE,
    ...) {
    time_start <- proc.time()  
     
    engine <- match.arg(tolower(engine), c("cpp", "r"))
    if(engine == "cpp" && 
       (is.function(model$transition_prob) ||
       is.function(model$observation_prob) ||
       is.function(model$reward))) {
      
      warning("Some elements of the POMDP are defined as R funciton. The CPP engine is very slow with R function calls.\n",
              "Falling back to R. Normalize the model first to use CPP.", immediate. = TRUE
              )
      engine <- 'r'
    }
    
    solved <- is_solved_POMDP(model)
    dt <- is_timedependent_POMDP(model)
    
    if (is.null(belief))
      belief <- start_vector(model)
    
    if (!is.numeric(belief) || length(belief) != length(model$states) || 
        !sum1(belief))
      stop("Initial belief is misspecified!")
    
    n <- as.integer(n)
    digits <- as.integer(digits)
    
    if (is.null(horizon))
      horizon <- model$horizon
    if (is.null(horizon) || is.infinite(horizon)) {
      if (is.null(model$discount) || !(model$discount < 1)) 
        stop("Simulation needs a finite simulation horizon.")
      
      # find a horizon that approximates the reward with a maximal error
      # of delta. The PV of the perpetuity of max_abs_R starting after t at gamma is 
      # PV = max_abs_R / gamma * gamma^t. For the PV <= delta, we need to solve
      # t >= log(delta/max_abs_R) / log(gamma) + 1
      # discount^horizon * max_abs_R <= 0.001
      max_abs_R <- .max_abs_reward(model)
      horizon <- ceiling(log(delta_horizon/max_abs_R)/log(model$discount)) + 1
    }
    horizon <- as.integer(horizon)
    
    # eps-greedy?
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
      
      ### TODO: check model!!! Also in simulate_MDP!
      
      if (!dt) {
        ### TODO: Add support for time dependence (C++)
        
        # Cpp can now deal with unnormalized POMDPs
        #model <- normalize_POMDP(model, sparse = TRUE)
        
        if (foreach::getDoParWorkers() == 1 || n * horizon < 100000L) {
          res <- simulate_POMDP_cpp(
            model,
            n,
            belief,
            horizon,
            disc,
            return_beliefs,
            return_trajectories,
            epsilon,
            digits,
            verbose
          )
          
          time_end <- proc.time()
          if (verbose)
            print(time_end - time_start)
          
          return(res)
        }
        
        ## parallel
        ns <- foreach_split(n)
      
        if (verbose) {
          cat("Simulating POMDP trajectories.\n")
          cat("- engine: cpp\n")
          cat("- horizon:", horizon, "\n")
          cat("- n:",
            n,
            "- parallel workers:",
            length(ns),
            "\n")
          cat("- epsilon:", epsilon, "\n")
          cat("- discount factor:", disc, "\n")
          cat("- starting belief:\n")
          print(head(belief, n = 10))
          if (length(belief) > 10) cat('(Remaining belief components supressed)')
          cat("\n")
        }
       
        w <-
          NULL # to shut up the warning for the foreach counter variable
        
        sim <- foreach(w = 1:length(ns)) %dopar%
          simulate_POMDP_cpp(model,
            ns[w],
            belief,
            horizon,
            disc,
            return_beliefs,
            return_trajectories,
            epsilon,
            digits,
            verbose = FALSE)
        
        
        # adjust the episode number for parallel processing
        episode_add <- cumsum(c(0L, ns))
        for (i in seq_along(sim)) {
          sim[[i]]$trajectories$episode <-
            sim[[i]]$trajectories$episode + episode_add[i]
        }
        
        rew <- Reduce(c,  lapply(sim, "[[", "reward"))
    
        time_end <- proc.time()
        if (verbose)
          print(time_end - time_start)
        
        return(
          list(
            avg_reward = mean(rew, na.rm = TRUE),
            reward = rew,
            action_cnt = Reduce('+', lapply(sim, "[[" , "action_cnt")),
            state_cnt =  Reduce('+', lapply(sim, "[[", "state_cnt")),
            obs_cnt =    Reduce('+', lapply(sim, "[[", "obs_cnt")),
            belief_states = Reduce(rbind, lapply(sim, "[[", "belief_states")),
            trajectories = Reduce(rbind, lapply(sim, "[[", "trajectories"))
          )
        )
        
      } else {
        message("Using engine 'r'. Time-dependent models can only be simulated using engine 'r'!")
        engine <- "r"
      }
    }
    
    ##### use R ############
    
    states <- as.character(model$states)
    n_states <- length(states)
    states_absorbing <- which(absorbing_states(model))
    obs <- as.character(model$observations)
    n_obs <- length(obs)
    actions <- as.character(model$actions)
    
    # precompute matrix lists for time-dependent POMDPs
    current_episode <- 1L
    
    if (dt) {
      dt_horizon <- model$horizon
      dt_episodes <- cumsum(c(1, head(model$horizon,-1)))
      dt_trans_m <-
        lapply(
          1:length(dt_horizon),
          FUN = function(ep)
            transition_matrix(model, ep)
        )
      dt_obs_m <-
        lapply(
          1:length(dt_horizon),
          FUN = function(ep)
            observation_matrix(model, ep)
        )
      ## we keep the reward matrix as is to save memory.
      # dt_rew_m <-
      #   lapply(
      #     1:length(dt_horizon),
      #     FUN = function(ep)
      #       reward_matrix(model, ep)
      #   )
      
      trans_m <- dt_trans_m[[current_episode]]
      obs_m <- dt_obs_m[[current_episode]]
      
    } else { ### not time-dependent case
    
      trans_m <- transition_matrix(model, sparse = NULL)
      obs_m <- observation_matrix(model, sparse = NULL)
    
      ## we keep the reward matrix as is to save memory.
      ## rew_m <- reward_matrix(model, sparse = NULL)
    }
    
    if (verbose) {
      cat("Simulating POMDP trajectories.\n")
      cat("- engine: r\n")
      cat("- horizon:", horizon, "\n")
      cat("- n:",
        n,
        "- parallel workers:",
        foreach::getDoParWorkers(),
        "\n")
      cat("- epsilon:", epsilon, "\n")
      if (dt)
        cat("- time-dependent:", length(dt_horizon), "episodes", "\n")
      cat("- discount factor:", disc, "\n")
      cat("- starting belief:\n")
      print(head(belief, n = 10))
      if (length(belief) > 10) cat('(Remaining belief components supressed)')
      cat("\n")
    }
    
    ### run once for debugging
    #bs <- replicate(n, expr = {
    #sim <- { cat("debugging on!!!\n")
    sim <- foreach(i = 1:n) %dopar% {
      # initialize replication
      alpha_vec_id <- NA_integer_
      s <- sample.int(length(states), 1L, prob = belief)
      b <- belief
      rew <- 0
      e <- 1L
      
      action_cnt <- rep(0L, length(actions))
      names(action_cnt) <- actions
      state_cnt <- rep(0L, length(states))
      names(state_cnt) <- states
      obs_cnt <- rep(0L, length(obs))
      names(obs_cnt) <- obs
      
      if (return_beliefs)
        visited_belief_states <- matrix(
          NA,
          nrow = horizon,
          ncol = n_states,
          dimnames = list(NULL, states)
        )
      else
        visited_belief_states <- matrix(nrow = 0, ncol = 0)
      
      if (return_trajectories)
        trajectory <- data.frame(
          episode = rep(NA_integer_, horizon),
          time = rep(NA_integer_, horizon),
          simulation_state = NA_integer_,
          alpha_vector_id = NA_integer_,
          a = NA_integer_,
          r = NA_real_
        )
      else
        trajectory <- NULL
      
      for (j in 1:horizon) {
        # change matrices for time-dependent POMDPs
        if (dt) {
          if (length(current_episode <- which(j == dt_episodes)) == 1L) {
            if (verbose)
              cat("- Switching to episode" , current_episode, "at epoch", j, "\n")
            obs_m <- dt_obs_m[[current_episode]]
            trans_m <- dt_trans_m[[current_episode]]
            #rew_m <- dt_rew_m[[current_episode]]
          }
        }
        
        # find action (if we have no solution then take a random action) and update state and sample obs
        if (runif(1) < epsilon) {
          a <- sample.int(length(actions), 1L)
        } else {
          if (!model$solution$converged)
            e <- .get_pg_index(model, j)
          
          alpha_vec_id <- which.max(model$solution$alpha[[e]] %*% b)
          a <-
            as.integer(model$solution$pg[[e]][["action"]])[alpha_vec_id]
        }
        
        # debug
        # cat("Episode: ", j, "\n")
        # cat("alpha: ", model$solution$alpha[[e]], "\n")
        # cat("b: ", b , "\n")
        # cat("id of winning alpha vector : ", alpha_vec_id , "\n")
        # cat("a: ", a , "\n\n")
        
        s_prev <- s
        s <-
          sample.int(length(states), 1L, prob = trans_m[[a]][s, ])
        o <- sample.int(length(obs), 1L, prob = obs_m[[a]][s, ])
        
        action_cnt[a] <- action_cnt[a] + 1L
        state_cnt[s] <- state_cnt[s] + 1L
        obs_cnt[o] <- obs_cnt[o] + 1L
        
        # rew <- rew + rew_m[[a]][[s_prev]][s, o] * disc ^ (j - 1L)
        r <- reward_matrix(model, a, s_prev, s, o, episode = current_episode)
        rew <- rew + r * disc ^ (j - 1L)
        #cat(j, ":", s_prev , "->", s, "- a:", a, "- o:", o, "- rew:", rew_m[[a]][[s_prev]][s, o], "\n")
        
        # update belief
        b <-
          .update_belief(b, a, o, trans_m, obs_m, digits = digits)
        
        if (return_beliefs)
          visited_belief_states[j,] <- b
        
        if (return_trajectories)
          trajectory[j, ] <-
          data.frame(
            episode = i,
            time = j - 1L,
            simulation_state = s_prev,
            alpha_vector_id = alpha_vec_id,
            a = a,
            r = r
          )
        
        if(s %in% states_absorbing) {
          if (return_trajectories)
            trajectory <- trajectory[1:j, , drop = FALSE]
          visited_belief_states <- visited_belief_states[1:j, , drop = FALSE]
          # TODO: maybe add the finals state
          break
        }
      }
      
      # terminal values
      if (j == sum(model$horizon) && !is.null(model$terminal_values)) {
        rew <- rew + model$terminal_values[s] * disc ^ j
      }
      
      rownames(visited_belief_states) <- NULL
      
        list(
          action_cnt = action_cnt,
          state_cnt = state_cnt,
          obs_cnt = obs_cnt,
          reward = rew,
          belief_states = visited_belief_states,
          trajectory = trajectory
      )
    }
    #, simplify = FALSE)
    
    time_end <- proc.time()
    if (verbose)
      print(time_end - time_start)
    
    rew <- Reduce(c,  lapply(sim, "[[", "reward"))
    
    trajectories <- NULL
    if (return_trajectories) {
      trajectories <- Reduce(rbind, lapply(sim, "[[", "trajectory"))
       trajectories$simulation_state <-
         factor(trajectories$simulation_state,
                levels = seq_along(states),
                labels = states)
      trajectories$a <-
        factor(trajectories$a,
               levels = seq_along(actions),
               labels = actions)
    }
    
    list(
      avg_reward = mean(rew, na.rm = TRUE),
      action_cnt = Reduce('+', lapply(sim, "[[" , "state_cnt")),
      state_cnt =  Reduce('+', lapply(sim, "[[", "state_cnt")),
      obs_cnt =    Reduce('+', lapply(sim, "[[", "obs_cnt")),
      reward = rew,
      belief_states = Reduce(rbind, lapply(sim, "[[", "belief_states")),
      trajectories = trajectories
    )
  }
