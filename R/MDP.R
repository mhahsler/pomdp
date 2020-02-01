# Class POMDP is a list with model and solution.
# Unsolved models have no solution element.

MDP <- function(
  states,
  actions,
  transition_prob,
  reward,
  discount = .9,
  horizon = Inf,
  terminal_values = 0,
  start = "uniform",
  max = TRUE,
  name = NA) {
  
  ### FIXME: Check the values!
  
  # discount shuold be a number in [0,1]
  # states should be a vector of strings
  # actions should be a vector of strings
  # observations should be a vector of strings
  # start should be either a vector of n numbers each in [0,1] that add up to 1 where n is the number of states
  # or the word "uniform", or a single number in 1 to n, or the name of a single state, or the names of a subset of states
 
  ### add names to start 
  if(is.numeric(start) && length(start) == length(states)) {
    if(is.null(names(start))) names(start) <- states
    else start <- start[states]
  }
  
  # transition_prob is either a list consisting of m matrices where m is the number of actions
  # or a data frame with 4 columns
  
  ### add names to transition probabilities
  for(a in names(transition_prob)) {
    if(is.matrix(transition_prob[[a]])) dimnames(transition_prob[[a]]) <- list(states, states)
  }  
 
  # Define a MDP using a POMDP 
  observations <- states
  identity_matrix <- diag(1, nrow = length(observations), ncol = length(states))
  observation_prob <- sapply(actions, FUN = function(a) identity_matrix, simplify = FALSE)
  
  # observation_prob is either a list consisting of m matrices where m is the number of actions 
  # or a data frame with 4 columns
  # reward should be either a matrix of size mxn where n is the number of states or 
  # a data frame with 5 columns
  # grid_size is an integer
  
  ### unsolved pomdp model
  structure(list(
    model = structure(list(
      name = name,
      discount = discount, 
      horizon = horizon,
      states = factor(states), 
      actions = factor(actions), 
      observations = factor(observations),  
      transition_prob = transition_prob,
      observation_prob = observation_prob, 
      reward = reward,
      start = start, 
      terminal_values = terminal_values,
      max = max
      ), class = "POMDP_model")
  ), class = c("MDP", "POMDP"))
}

