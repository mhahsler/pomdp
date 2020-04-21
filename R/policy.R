#' Extract the Policy from a POMDP/MDP
#' 
#' Extracts the policy from a solves POMDP/MDP.
#' 
#' A list (one entry per epoch) with the optimal policy. The policy is a data
#' frame consisting of three parts.
#' 
#' Part 1: The value function with one column per state. (For MDPs this is just
#' one column with the state).
#' 
#' Part 2: One column with the optimal action.
#' 
#' Part 3: One column per observation with the index of the row representing
#' the policy node in the next epoch.
#' 
#' @param x A solved POMDP object.
#' @return A list with the policy for each epoch. %% ~Describe the value
#' returned %% If it is a LIST, use %% \item{comp1 }{Description of 'comp1'} %%
#' \item{comp2 }{Description of 'comp2'} %% ...
#' @author Michael Hahsler
#' @keywords graphs
#' @examples
#' 
#' data("Tiger")
#' sol <- solve_POMDP(model = Tiger)
#' sol
#' 
#' # policy with value function, optimal action and transitions for observations.
#' policy(sol)
#' 
#' @export
policy <- function(x) {
  if(inherits(x, "MDP")) .policy_MDP(x)
  else .policy_POMDP(x)
}

.policy_POMDP <- function(x) {
  .solved_POMDP(x)
  
  pg <- x$solution$pg
  alpha <- x$solution$alpha
  
  for(i in 1:length(pg)) {  
    pg[[i]] <- cbind(alpha[[i]],
      pg[[i]][,-1])
  }
  
  pg
}

# FIXME: This is a MDP policy
  
# create belief states to MDP (identity matrix)
.MDP_belief_states <- function(model, projection = NULL) {
  belief_states <- diag(1, nrow = length(model$model$states), ncol = length(model$model$states))
  colnames(belief_states) <- model$model$states
  if(!is.null(projection)) {
    # FIXME: convert state names!
    belief_states <- belief_states[projection,]
  }
  
  belief_states
}

.policy_MDP <- function(x) {
  .solved_POMDP(x)

  pg <- x$solution$pg
  bs <- .MDP_belief_states(x)
  
  for(i in 1:length(pg)) {  
    rew <- reward(x, belief = bs, epoch = i)
    pg[[i]] <- pg[[i]][rew$pg_node,]
    pg[[i]] <- cbind(data.frame(state = colnames(bs), reward = rew$reward),
      pg[[i]][,-1])
  }
  
  pg
}
