
## write a model in POMDP format for pomdp-solve
write_POMDP <- function(model, file) {
  if(!is(model, "POMDP_model")) stop("model needs to be a POMDP model use POMDP()!")
  
  discount    <- model$discount 
  states      <- model$states 
  actions     <- model$actions 
  observations <- model$observations 
  start       <- model$start 
  transition_prob <- model$transition_prob
  observation_prob <- model$observation_prob 
  reward      <- model$reward
  values      <- model$values
  
  ### POMDP file
  code <- character()
  code <-  paste(c("discount:", discount, "\n") , collapse = " ")
  
  # deal with rewards or costs
  values <- match.arg(values, choices = c("reward", "cost"))
  code <- paste(c(code,"values:", values, "\n"), collapse = " ")
  
  # counting the number of states
  number_of_states <- length(states)
  
  code <- paste(c(code,"states:", states, "\n"), collapse = " ")
  
  # counting the number of actions
  number_of_actions <- length(actions)
  
  code <- paste(c(code,"actions:", actions, "\n"), collapse = " ")
  
  # counting the number of observations
  number_of_observations <- length(observations)
  
  code <- paste(c(code,"observations:", observations, "\n"), collapse = " ")
  
  ### starting beliefs
  
  ## if the starting beliefs are given by enumerating the probabilities for each state
  if (!is.character(start)) {
    if (sum(start)==1) {
      code <- paste(c(code,"start:", start, "\n"), collapse = " ")
    }
  }
  ## if the starting beliefs are given by a uniform distribution over all states
  if (sum(start== "uniform")==1) {
    code <- paste(c(code,"start:", start, "\n"), collapse = " ")
  } else if (start[1]!="-") {  ## if the starting beliefs include a specific subset of states
    # if the starting beliefs are given by a uniform distribution over a subset of states (using their names)
    if (!is.na(sum(match(start, states)))) {
      code <- paste(c(code, "start include:", start, "\n"), collapse = " ")
    }
    # if the starting beliefs are given by a uniform distribution over a subset of states (using their numbers)
    if (!is.character(start)) { 
      if (sum(start)>=1 & length(start)<number_of_states) {
        code <- paste(c(code, "start include:", start, "\n"), collapse = " ")
      }
    }
  } else if (start[1]=="-") { ## if the starting beliefs exclude a specific subset of states
    code <- paste(c(code, "start exclude:", start[-1], "\n"), collapse = " ")
  }
  
  
  ### Transition Probabilities
  
  ## if the transition probabilities are given in the general form
  if (is.data.frame(transition_prob)) {
    # checking if the number of the columns of the given data frame is 4
    if (dim(transition_prob)[2] != 4) {
      stop("the given data frame for the transition probabilities needs to have 4 columns including 'action', 'start-state','end-state','probability'")
    }
    # writing the transition probabilitie lines
    for (i in 1:dim(transition_prob)[1]) {
      code <- paste(c(code,"T:", 
        as.character(transition_prob[i,1]), ":", 
        as.character(transition_prob[i,2]), ":", 
        as.character(transition_prob[i,3]), 
        transition_prob[i,4],  "\n"), collapse = " ")
    }
  }
  
  
  ## if the transition probabilities are given in the form of action dependent matrices
  if (!is.data.frame(transition_prob)) {
    # checking if the number of the given transition probability matrices matches the number of actions
    if (length(transition_prob)!=number_of_actions) {
      stop("the number of given transition probability matrices does not match the number of actions")
    }
    # writing the transition probability matrices
    for (i in 1:number_of_actions) {
      code <- paste(c(code,"T:", actions[i], "\n"), collapse = " ")
      if (any(transition_prob[[actions[i]]] == "uniform") | 
          any(transition_prob[[actions[i]]] == "identity")) {
        code <- paste(c(code, transition_prob[[actions[i]]], "\n"), collapse = " ")
      } 
      else {
        c_m <- character()
        for (j in 1:number_of_states) {
          c_m <- paste(c(c_m, transition_prob[[actions[i]]][j,], "\n"), collapse = " ")
        }
        code <- paste(c(code, c_m), collapse = " ")
      }
    }
  }
  
  
  ### Observation Probabilities
  
  ## if the observation probabilities are given in the general form
  if (is.data.frame(observation_prob)) {
    # checking if the number of the columns of the given data frame is 4
    if (dim(observation_prob)[2] != 4) {
      stop("the given data frame for the observation probabilities needs to have 4 columns including 'action', 'end-state','observation','probability'")
    }
    # writing the transition probabilities lines
    for (i in 1:dim(observation_prob)[1]) {
      code <- paste(c(code,"O:", 
        as.character(observation_prob[i,1]), ":", 
        as.character(observation_prob[i,2]), ":", 
        as.character(observation_prob[i,3]), 
        observation_prob[i,4],  "\n"), collapse = " ")
    }
  }
  
  ## if the observation probabilities are given in the form of action dependent matrices
  if (!is.data.frame(observation_prob)) {
    # checking if the number of the given observation probability matrices matches the number of actions
    if (length(observation_prob)!=number_of_actions) {
      stop("the number of given observation probability matrices does not match the number of actions")
    }
    # writing the observation probability matrices
    for (i in 1:number_of_actions) {
      code <- paste(c(code,"O:", actions[i], "\n"), collapse = " ")
      if (any(observation_prob[[actions[i]]] == "uniform") | 
          any(observation_prob[[actions[i]]] == "identity")) {
        code <- paste(c(code, observation_prob[[actions[i]]] , "\n"), collapse = " ")
      } 
      else {
        c_m <- character()
        for (j in 1:number_of_states) {
          c_m <- paste(c(c_m, observation_prob[[actions[i]]][j,], "\n"),collapse = " ")
        }
        code <- paste(c(code, c_m), collapse = " ")
      }
    }
  }
  
  ### Rewards/Costs
  
  ## if the rewards are given in the general form
  if (is.data.frame(reward)) {
    # checking if the number of the columns of the given data frame is 5
    if (dim(reward)[2] != 5) {
      stop("the given data frame for the Rewards needs to have 5 columns including 'action', 'start-state','end-state','observation', 'values'")
    }
    # writing the reward lines
    for (i in 1:dim(reward)[1]) {
      code <- paste(c(code,"R:", 
        as.character(reward[i,1]), ":", 
        as.character(reward[i,2]), ":", 
        as.character(reward[i,3]), ":", 
        as.character(reward[i,4]), 
        reward[i,5],  "\n"), collapse = " ")
    }
  }
  
  ## if the rewards are given in the form of action-and-start-state dependent matrix
  if (!is.data.frame(reward)) {
    # writing the reward section
    for (i in 1:number_of_actions) {
      for (j in 1:number_of_states) {
        code <- paste(c(code, "R:" , actions[i], ":" , states[j], "\n"))
        c_m <- paste(rep(paste(c(rep(reward[i,j], number_of_observations), "\n"),
          collapse = " "), number_of_states), collapse = " ")
        code <- paste(c(code, c_m),collapse = " ")
      }
    }
  }
  
  
  ### saving the POMDP file
  cat(code, file = file)
}
