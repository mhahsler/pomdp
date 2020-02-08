
.MDP_belief_states <- function(model, projection = NULL) {
  belief_states <- diag(1, nrow = length(model$model$states), ncol = length(model$model$states))
  colnames(belief_states) <- model$model$states
  if(!is.null(projection)) {
    # FIXME: convert state names!
    belief_states <- belief_states[projection,]
  }
  
  belief_states
}

policy <- function(x) {
  if(inherits(x, "MDP")) .policy_MDP(x)
  else .policy_POMDP(x)
}

### FIXME: This is a MDP policy
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