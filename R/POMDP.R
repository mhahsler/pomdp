# Class POMDP is a list with model and solution.
# Unsolved models have no solution element.

POMDP <- function(
  discount = 0,
  states,
  actions,
  observations,
  transition_prob,
  observation_prob,
  reward,
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
      states = states, 
      actions = actions, 
      observations = observations,  
      start = start, 
      transition_prob = transition_prob,
      observation_prob = observation_prob, 
      reward = reward,
      max = max
      ), class = "POMDP_model")
  ), class = "POMDP")
}

print.POMDP <- function(x, ...) {
  if(is.null(x$solution)) cat("Unsolved POMDP model:", x$model$name, "\n")
  else cat(
    "Solved POMDP model:", x$model$name, "\n", 
    "\thorizon:", x$solution$horizon, "\n",
    "\tsolution method:", x$solution$method, "\n",
    "\tpolicy graph nodes:", nrow(x$solution$pg), "\n",
    "\ttotal expected reward:", x$solution$total_expected_reward, "\n\n" 
  )
}

print.POMDP_model <- function(x, ...) {
 cat("POMDP model:", x$name, "\n\n")
 if(!is.null(x$problem)) { 
   cat("Model specification from POMDP file\n\n")
   cat("--------------- Start ---------------\n")
   cat(x$problem, sep = "\n")
   cat("---------------- End ----------------\n\n")
   x$problem <- NULL
  }
 print(unclass(x))
}

model <- function(x) {
  if(!inherits(x, "POMDP")) stop("x needs to be a POMDP object!")
  x$model 
}

# check if x is a solved POMDP
.solved_POMDP <- function(x) {
  if(!inherits(x, "POMDP")) stop("x needs to be a POMDP object!")
  if(is.null(x$solution)) stop("x needs to be a solved POMDP. Use solve_POMDP() first.")
}

solution <- function(x) {
  .solved_POMDP(x)
  x$solution
}



### helper
O_ <- function(action, end.state, observation, probability) 
  data.frame(action = action, end.state = end.state, observation = observation, probability = probability)

R_ <- function(action, start.state, end.state, observation, value) 
  data.frame(action = action, start.state = start.state, end.state = end.state, 
    observation = observation, value = value)

T_ <- function(action, start.state, end.state, probability) 
  data.frame(action = action, start.state = start.state, end.state= end.state, probability = probability)
