#' # Updating the belief state
#'
#' $$b'(s') = \eta O(o | s',a) \sum_{s \in S} T(s' | s,a) b(s)$$
#' $$\eta = 1/ \sum_{s' \in S}[ O(o | s',a) \sum_{s \in S} T(s' | s,a) b(s)]$$

round_fixed <- function(x, digits) round(x*10^digits) / 10^digits

# update for a single belief vector, one action, and one observation.
.update_belief <- function(belief, action, observation, Tr, Ob, digits = 7) {
    belief <- Ob[[action]][, observation, drop = FALSE] * crossprod(Tr[[action]], cbind(belief))
    belief <- belief/sum(belief)
    belief <- round_fixed(belief, digits)
    drop(belief)
}

#' # Update the belief using an action and and observation   
#'
#' If no action or observation is specified, all reachable belief states for the next epoch
#' are calculated
update_belief <- function(model, belief = NULL, action = NULL, observation = NULL, 
  episode = 1, digits = 7, drop = TRUE){
  
  # belief has to be a single row vector
  belief <- .translate_belief(belief, model = model)
  if(!is.vector(belief)) stop("belief has to be specified as a numeric vector.")
  
  Ob <- observation_matrix(model, episode = episode)
  Tr <- transition_matrix(model, episode = episode)
  
  if(is.null(action)) action <- as.character(model$model$actions)
  if(is.null(observation)) observation <- as.character(model$model$observations)
  
  .update_vec <- Vectorize(.update_belief, vectorize.args = c("action", "observation"), SIMPLIFY = TRUE)
  
  g <- expand.grid(action, observation, stringsAsFactors = FALSE)
  
  b <- t(.update_vec(belief, g[,1], g[,2], Tr, Ob, digits))  
  rownames(b) <- apply(g, MARGIN = 1, paste, collapse = "+")
  colnames(b) <- as.character(model$model$states)
  
  if(drop) b <- drop(b)
  b
}

#' # Simulate belief points
#' 
#' If we have a solution, the policy is followed. Otherwise, a random action is chosen. 

simulate_POMDP <- function(model, n = 100, belief = NULL, horizon = NULL, 
  visited_beliefs = FALSE, random_actions = FALSE, digits = 7, verbose = FALSE) {
  
  belief <- .translate_belief(belief, model = model)
  solved <- !is.null(model$solution)

  if(is.null(horizon)) horizon <- model$solution$horizon
  if(is.null(horizon)) horizon <- model$model$horizon
  if(is.null(horizon)) stop("The horizon (number of epochs) has to be specified!")
  if(is.infinite(horizon)) stop("Simulation needs a finite simulation horizon.")
  
  if(!is.null(random_actions)) {
    random_actions <- as.logical(random_actions)
    if(!solved && !random_actions) stop("Only random actions are possible for unsolved POMDPs.")
    solved <- !random_actions
  }
  
  disc <- if(solved) model$solution$discount else model$model$discount
  if(is.null(disc)) disc <- 1

  states <- as.character(model$model$states)
  n_states <- length(states)
  obs <- as.character(model$model$observations)
  n_obs <- length(obs)
  actions <- as.character(model$model$actions)
  
  trans_m <- transition_matrix(model)
  obs_m <- observation_matrix(model)
  rew_m <- reward_matrix(model)
  
  # precompute matrix lists for time-dependent POMDPs
  dt <- .timedependent_POMDP(model)
  if(dt) {
    dt_horizon <- model$model$horizon
    dt_episodes <- cumsum(c(1, head(model$model$horizon, -1)))
    dt_trans_m <- lapply(1:length(dt_horizon), FUN = function(ep) transition_matrix(model, ep))
    dt_obs_m <- lapply(1:length(dt_horizon), FUN = function(ep) observation_matrix(model, ep))
    dt_rew_m <- lapply(1:length(dt_horizon), FUN = function(ep) reward_matrix(model, ep))
  }
  
  if(verbose) {
    cat("Simulating POMDP trajectories.\n")
    cat("- using optimal actions:", solved, "\n")
    cat("- horizon:", horizon, "\n")
    if(dt) cat("- time-dependent:", length(dt_horizon), "episodes", "\n")
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
      rew <- 0
        
      if(visited_beliefs) b_all <- matrix(NA, nrow = horizon, ncol = n_states, dimnames = list(NULL, states))
      
      for(j in 1:horizon) {
        # change matrices for time-dependent POMDPs
        if(dt) {
          if(length(new_ep <- which(j == dt_episodes)) == 1L) {
            #cat("- Switching to episode" , new_ep, "at epoch", j, "\n")
            obs_m <- dt_obs_m[[new_ep]]
            trans_m <- dt_trans_m[[new_ep]]
            rew_m <- dt_rew_m[[new_ep]]
          }
        }
        
        # find action (if we have no solution then take a random action) and update state and sample obs
        
        #if(solved) {
        # e <- .get_pg_index(model, j)
        # a <- as.character(reward(model, b, e)$action)
        #}else a <- sample(actions, 1)
        # this takes about 1/2 the time
        if(solved) {
          #  convert intex for converged POMDPs 
          e <- .get_pg_index(model, j)
          a <- as.character(model$solution$pg[[e]][
            which.max(model$solution$alpha[[e]] %*% b), "action"])
        } else a <- sample(actions, 1)
          
        action_cnt[a] <- action_cnt[a] + 1L
        s_prev <- s
       
        s <- sample(states, 1, prob = trans_m[[a]][s,])
        o <- sample(obs, 1, prob = obs_m[[a]][s,])
        
        rew <- rew + rew_m[[a]][[s_prev]][s, o] * disc^(j-1L)
  
        #cat(j, ":", s_prev , "->", s, "- a:", a, "- o:", o, "- rew:", rew_m[[a]][[s_prev]][s, o], "\n")
        
              
        # update belief
        b <- .update_belief(b, a, o, trans_m, obs_m, digits)
        if(visited_beliefs) b_all[j, ] <- b
      }
      
      if(!visited_beliefs) b_all <- b
      
      rownames(b_all) <- NULL
      attr(b_all, "action_cnt") <- action_cnt
      attr(b_all, "reward") <- rew
      b_all
      
    }, simplify = FALSE
  )

  ac <- Reduce(rbind, lapply(bs, attr, "action_cnt"))
  rownames(ac) <- NULL
  rew <- Reduce(rbind, lapply(bs, attr, "reward"))
  rownames(rew) <- NULL
  bs <- Reduce(rbind, bs) 
  rownames(bs) <- NULL
  
  attr(bs, "action_cnt") <- ac
  attr(bs, "reward") <- rew
  attr(bs, "avg_reward") <- mean(rew, na.rm = TRUE)
  
  bs
}

