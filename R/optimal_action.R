# find the optimal action for a belief point

optimal_action <- function(model, belief, epoch = 1) {
  .solved_POMDP()
  
  belief <- .translate_belief(belief, model = model)
  e <- .get_pg_index(model, epoch)
  
  a <- as.character(model$solution$pg[[e]][
    which.max(model$solution$alpha[[e]] %*% belief), "action"])
  a
}
