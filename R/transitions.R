
tr_unif <- function(states) 
  matrix(1/length(states), nrow = length(states), ncol = length(states), 
    dimnames = list(states, states))

tr_identity <- function(states) { 
  d <- diag(length(states))
  dimnames(d) <- list(states, states)
  d
  }

tr_0 <- function(states) 
  matrix(0, nrow = length(states), ncol = length(states), 
    dimnames = list(states, states))


transitions <- function(model) {
  if(inherits(model, "POMDP")) model <- model(model)
  if(!inherits(model, "POMDP_model")) stop("model needs to be an object of class POMDP or POMDP_model.")

  trans <- model$transition_prob
  actions <- model$actions
  states <- model$states
   
  if(is.list(trans)) {
    for(a in actions) {
      if(is.character(trans[[a]]) && trans[[a]] == "identity") 
        trans[[a]] <- tr_identity(states)
      if(is.character(trans[[a]]) && trans[[a]] == "uniform") 
        trans[[a]] <- tr_unif(states)
    }
    
  } else if(is.data.frame(trans)) {
    l <- lapply(actions, FUN = function(a) tr_0(states))
    names(l) <- actions
    
    for(i in 1:nrow(trans)) {
      l[[trans[i, 1]]][trans[i, 2]][trans[i, 3]] <- trans[i, 4]
    }
    
    trans <- l
    
  } else stop("Unknown format for transition probabilities.")

  trans
}
