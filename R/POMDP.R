# Class POMDP is a list with model and solution.
# Unsolved models have no solution element.

POMDP <- function(
  states,
  actions,
  observations,
  transition_prob,
  observation_prob,
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
  ), class = "POMDP")
}

print.POMDP <- function(x, ...) {
  if(is.null(x$solution)) cat(
    "Unsolved POMDP model:", x$model$name, "\n",
    "\thorizon:", paste(x$model$horizon, collapse = "+"), "\n",
    if(!is.null(x$model$horizon) && length(x$model$horizon) > 1) paste("\ttime-dependent:", length(x$model$horizon), "episodes\n") 
    )
  else cat(
    "Solved POMDP model:", x$model$name, "\n", 
    "\tsolution method:", x$solution$method, "\n",
    "\thorizon:", x$solution$horizon, "\n",
    if(!is.null(x$model$horizon) && length(x$model$horizon) > 1) paste("\ttime-dependent:", length(x$model$horizon), "episodes\n") 
    else "",
    "\tconverged:", x$solution$converged, "\n",
    "\ttotal expected reward (for start probabilities):", x$solution$total_expected_reward, "\n" 
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


# check if x is a solved POMDP
.solved_POMDP <- function(x) {
  if(!inherits(x, "POMDP")) stop("x needs to be a POMDP object!")
  if(is.null(x$solution)) stop("x needs to be a solved POMDP. Use solve_POMDP() first.")
}

.timedependent_POMDP <- function(x) {
  !is.null(x$model$horizon) && length(x$model$horizon) > 1L
}

# get pg and alpha for a epoch
.get_pg_index <- function(model, epoch) {
  .solved_POMDP(model)
  
  if(epoch < 1) stop("Epoch has to be >= 1")
  
  h <- model$solution$horizon
  l <- length(model$solution$pg)
  
  if(epoch > h) 
    stop("POMDP model was only solved for ", h, " epochs!")
  
  ### (converged) infinite horizon POMDPs
  if(is.infinite(h)) epoch <- 1L
  
  ### converged finite horizon model
  else {
    if(epoch <= h-l) epoch <- 1L
    else epoch <- epoch-(h-l) 
  }
  
  epoch  
}

.get_pg <- function(model, epoch) model$solution$pg[[.get_pg_index(model, epoch)]]
.get_alpha <- function(model, epoch) model$solution$alpha[[.get_pg_index(model, epoch)]]
    

### helper
O_ <- function(action = "*", end.state = "*", observation = "*", probability) 
  data.frame(action = action, end.state = end.state, observation = observation, probability = probability)

T_ <- function(action = "*", start.state = "*", end.state = "*", probability) 
  data.frame(action = action, start.state = start.state, end.state= end.state, probability = probability)

R_ <- function(action = "*", start.state = "*", end.state = "*", observation = "*", value) 
  data.frame(action = action, start.state = start.state, end.state = end.state, 
    observation = observation, value = value)