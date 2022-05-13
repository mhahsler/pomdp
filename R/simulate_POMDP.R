## TODO: Reimplement in C++

#' Simulate Trajectories in a POMDP
#'
#' Simulate trajectories through a POMDP. The start state for each
#' trajectory is randomly chosen using the specified belief. For solved POMDPs
#' the optimal actions will be chosen, for unsolved POMDPs random actions will
#' be used.
#'
#' @family POMDP
#' @importFrom stats runif
#'
#' @param model a POMDP model.
#' @param n number of trajectories.
#' @param belief probability distribution over the states for choosing the
#' starting states for the trajectories.
#' Defaults to the start belief state specified in
#' the model or "uniform".
#' @param horizon number of epochs for the simulation. If `NULL` then the
#' horizon for the model is used.
#' @param visited_beliefs logical; Should all belief points visited on the
#' trajectories be returned? If `FALSE` then only the belief at the final
#' epoch is returned.
#' @param epsilon the probability of random actions  for using an epsilon-greedy policy.
#' Default for solved models is 0 and for unsolved model 1.
#' @param digits round belief points.
#' @param verbose report used parameters.
#' @return A matrix with belief points (in the final epoch or all) as rows. Attributes containing action
#' counts, and rewards  for each trajectory may be available.
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
#' ## Example 1: simulate 10 trajectories, only the final belief state is returned
#' sim <- simulate_POMDP(sol, n = 100, verbose = TRUE)
#' head(sim)
#'
#' # plot the final belief state, look at the average reward and how often different actions were used.
#' plot_belief_space(sol, sample = sim)
#'
#' # additional data is available as attributes
#' names(attributes(sim))
#' attr(sim, "avg_reward")
#' colMeans(attr(sim, "action"))
#'
#'
#' ## Example 2: look at all belief states in the trajectory starting with an initial start belief.
#' sim <- simulate_POMDP(sol, n = 100, belief = c(.5, .5), visited_beliefs = TRUE)
#'
#' # plot with added density
#' plot_belief_space(sol, sample = sim, ylim = c(0,5), jitter = 1)
#' lines(density(sim[, 1], bw = .02)); axis(2); title(ylab = "Density")
#'
#'
#' ## Example 3: simulate trajectories for an unsolved POMDP which uses a epsilon of 1 
#' # (i.e., all randomized actions)
#' sim <- simulate_POMDP(Tiger, n = 100, horizon = 5, visited_beliefs = TRUE)
#' plot_belief_space(sol, sample = sim, ylim = c(0,6))
#' lines(density(sim[, 1], bw = .05)); axis(2); title(ylab = "Density")
#' @export
simulate_POMDP <-
  function(model,
    n = 100,
    belief = NULL,
    horizon = NULL,
    visited_beliefs = FALSE,
    epsilon = NULL,
    digits = 7,
    verbose = FALSE) {
    belief <- .translate_belief(belief, model = model)
    solved <- !is.null(model$solution)
    
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
    obs <- as.character(model$observations)
    n_obs <- length(obs)
    actions <- as.character(model$actions)
    
    trans_m <- transition_matrix(model)
    obs_m <- observation_matrix(model)
    rew_m <- reward_matrix(model)
    
    # precompute matrix lists for time-dependent POMDPs
    dt <- .timedependent_POMDP(model)
    if (dt) {
      dt_horizon <- model$horizon
      dt_episodes <- cumsum(c(1, head(model$horizon, -1)))
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
      dt_rew_m <-
        lapply(
          1:length(dt_horizon),
          FUN = function(ep)
            reward_matrix(model, ep)
        )
    }
    
    if (verbose) {
      cat("Simulating POMDP trajectories.\n")
      cat("- horizon:", horizon, "\n")
      cat("- epsilon:", epsilon, "\n")
      if (dt)
        cat("- time-dependent:", length(dt_horizon), "episodes", "\n")
      cat("- discount factor:", disc, "\n")
      cat("- starting belief:\n")
      print(belief)
      cat("\n")
    }
    
    bs <- replicate(n, expr = {
      # find a initial state
      
      s <- sample(states, 1, prob = belief)
      b <- belief
      
      action_cnt <- rep(0L, length(actions))
      names(action_cnt) <- actions
      
      state_cnt <- rep(0L, length(states))
      names(state_cnt) <- states
      
      rew <- 0
      
      if (visited_beliefs)
        b_all <- matrix(
          NA,
          nrow = horizon,
          ncol = n_states,
          dimnames = list(NULL, states)
        )
      
      for (j in 1:horizon) {
        # change matrices for time-dependent POMDPs
        if (dt) {
          if (length(new_ep <- which(j == dt_episodes)) == 1L) {
            #cat("- Switching to episode" , new_ep, "at epoch", j, "\n")
            obs_m <- dt_obs_m[[new_ep]]
            trans_m <- dt_trans_m[[new_ep]]
            rew_m <- dt_rew_m[[new_ep]]
          }
        }
        
        # find action (if we have no solution then take a random action) and update state and sample obs
        
        if (runif(1) < epsilon) {
          a <- sample(actions, 1)
        } else {
          #  convert index for converged POMDPs
          e <- .get_pg_index(model, j)
          a <-
            as.character(model$solution$pg[[e]][which.max(model$solution$alpha[[e]] %*% b), "action"])
        }
        
        action_cnt[a] <- action_cnt[a] + 1L
        state_cnt[s] <- state_cnt[s] + 1L
        
        s_prev <- s
        s <- sample(states, 1, prob = trans_m[[a]][s,])
        o <- sample(obs, 1, prob = obs_m[[a]][s,])
        
        rew <- rew + rew_m[[a]][[s_prev]][s, o] * disc ^ (j - 1L)
        
        #cat(j, ":", s_prev , "->", s, "- a:", a, "- o:", o, "- rew:", rew_m[[a]][[s_prev]][s, o], "\n")
        
        
        # update belief
        b <- .update_belief(b, a, o, trans_m, obs_m, digits)
        if (visited_beliefs)
          b_all[j, ] <- b
      }
      
      if (!visited_beliefs)
        b_all <- b
      
      rownames(b_all) <- NULL
      attr(b_all, "action_cnt") <- action_cnt
      attr(b_all, "state_cnt") <- state_cnt
      attr(b_all, "reward") <- rew
      b_all
      
    }, simplify = FALSE)
    
    ac <- Reduce(rbind, lapply(bs, attr, "action_cnt"))
    rownames(ac) <- NULL
    sc <- Reduce(rbind, lapply(bs, attr, "state_cnt"))
    rownames(sc) <- NULL
    rew <- Reduce(rbind, lapply(bs, attr, "reward"))
    rownames(rew) <- NULL
    bs <- Reduce(rbind, bs)
    rownames(bs) <- NULL
    
    attr(bs, "action_cnt") <- ac
    attr(bs, "state_cnt") <- sc
    attr(bs, "reward") <- rew
    attr(bs, "avg_reward") <- mean(rew, na.rm = TRUE)
    
    bs
  }
