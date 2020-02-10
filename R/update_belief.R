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
  digits = 7, drop = TRUE){
  
  # belief has to be a single row vector
  belief <- .translate_belief(belief, model = model)
  if(!is.vector(belief)) stop("belief has to be specified as a numeric vector.")
  
  Ob <- observation_matrix(model)
  Tr <- transition_matrix(model)
  
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

simulate_POMDP <- function(model, n = 100, belief = NULL, epochs = NULL, 
  visited_beliefs = FALSE, digits = 7, verbose = FALSE) {

  if(is.null(epochs)) epochs <- model$solution$horizon
  if(is.null(epochs)) stop("The number of epochs has to be specified!")
   
  belief <- .translate_belief(belief, model = model)
  solved <- !is.null(model$solution)
  
  states <- as.character(model$model$states)
  n_states <- length(states)
  trans_m <- transition_matrix(model)
  
  obs <- as.character(model$model$observations)
  n_obs <- length(obs)
  obs_m <- observation_matrix(model)
  
  rew_m <- reward_matrix(model)
  disc <- if(solved) model$solution$discount else model$model$discount
  if(is.null(disc)) disc <- 1
   
  actions <- as.character(model$model$actions)
  
  if(verbose) {
    cat("Simulating POMDP trajectories.\n")
    cat("- using optimal actions:", solved, "\n")
    cat("- epochs:", epochs, "\n")
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
        
      if(visited_beliefs) b_all <- matrix(NA, nrow = epochs, ncol = n_states, dimnames = list(NULL, states))
      
      for(j in 1:epochs) {
        # find action (if we have no solution then take a random action) and update state and sample obs
        
        #if(solved) a <- as.character(reward(model, b, j)$action)
        #else a <- sample(actions, 1)
        # this takes about 1/2 the time
        if(solved) a <- as.character(model$solution$pg[[j]][
          which.max(model$solution$alpha[[j]] %*% b), "action"])
        else a <- sample(actions, 1)
          
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

