
## write a model in POMDP format for pomdp-solve
write_POMDP <- function(model, file) {
  if(!inherits(model, "POMDP")) stop("model needs to be a POMDP model use POMDP()!")
  
  model <- model$model
  
  discount    <- model$discount 
  states      <- model$states 
  number_of_states <- length(states)
  actions     <- model$actions 
  number_of_actions <- length(actions)
  observations <- model$observations 
  number_of_observations <- length(observations)
  start       <- model$start 
  transition_prob <- model$transition_prob
  observation_prob <- model$observation_prob 
  reward      <- model$reward
  max         <- model$max
  values <- ifelse(max, "reward", "cost")
  
  ### POMDP file
  code <-  paste0(
    "# POMDP File: ", model$name, "\n",
    "# Produced with R package pomdp\n",
    "\n",
    "discount: ", discount, "\n",
    "values: ", values, "\n",
    "states: ", paste(states, collapse = " "), "\n",
    "actions: ", paste(actions, collapse = " "), "\n",
    "observations: ", paste(observations, collapse = " "), "\n"
  )
  
  
  ### starting beliefs
  if(!is.null(start)) { 
    ## if the starting beliefs are given by enumerating the probabilities for each state
    if (!is.character(start)) {
      if (length(start) == length(states) && sum(start)==1) {
        code <- paste0(code,"start: ", paste(start, collapse = " "), "\n")
      }
    }
    ## if the starting beliefs are given by a uniform distribution over all states
    if (length(start) == 0 && start[1] == "uniform") {
      code <- paste(c(code,"start:", start, "\n"), collapse = " ")
    } else if (start[1] != "-") {  ## if the starting beliefs include a specific subset of states
      # if the starting beliefs are given by a uniform distribution over a subset of states (using their names)
      if (!any(is.na(match(start, states)))) {
        code <- paste0(code, "start include: ", paste(start, collapse = " "), "\n")
      }
      # if the starting beliefs are given by a uniform distribution over a subset of states (using their numbers)
      if (is.numeric(start)) { 
        start <- as.integer(start) -1L ### pomdp-solve starts with index 0
        if (all(start >= 0 & start < number_of_states) && length(start) <= number_of_states) {
          code <- paste0(code, "start include: ", paste(start, collapse = " "), "\n")
        }
      }
    } else if (start[1] == "-") { ## if the starting beliefs exclude a specific subset of states
      code <- paste0(code, "start exclude: ", paste(start[-1], collapse = " "), "\n")
    }
  }
  
  code <- paste0(code, "\n")
  
  ### Transition Probabilities
  
  ## if the transition probabilities are given in the general form
  if (is.data.frame(transition_prob)) {
    # checking if the number of the columns of the given data frame is 4
    if (ncol(transition_prob) != 4) {
      stop("the given data frame for the transition probabilities needs to have 4 columns including 'action', 'start-state','end-state','probability'")
    }
    
    # writing the transition probability lines
    for (i in 1:nrow(transition_prob)) {
      # fix indexing
      if(is.numeric(transition_prob[i,1])) transition_prob[i,1] <- transition_prob[i,1] -1    
      if(is.numeric(transition_prob[i,2])) transition_prob[i,1] <- transition_prob[i,1] -1    
      if(is.numeric(transition_prob[i,3])) transition_prob[i,1] <- transition_prob[i,1] -1    
      
      code <- paste0(code,"T: ", 
                      transition_prob[i,1], " : ", 
                      transition_prob[i,2], " : ", 
                      transition_prob[i,3], " ",
                      format(transition_prob[i,4], scientific = FALSE),  
        "\n")
    }
  }else{
    ## if the transition probabilities are given in the form of action dependent matrices
    # checking if the number of the given transition probability matrices matches the number of actions
    if (length(transition_prob)!=number_of_actions) {
      stop("the number of given transition probability matrices does not match the number of actions")
    }
    # writing the transition probability matrices
    for (i in 1:number_of_actions) {
      code <- paste0(code, "T: ", actions[i], "\n")
      
      if (is.character(transition_prob[[actions[i]]]) && length(transition_prob[[actions[i]]]) == 1){
        code <- paste0(code, transition_prob[[actions[i]]], "\n")
      } 
      else {
        c_m <- character()
        for (j in 1:number_of_states) {
          c_m <- paste(c(c_m, format(transition_prob[[actions[i]]][j,], scientific = FALSE), "\n"), 
            collapse = " ")
        }
        code <- paste(c(code, c_m), collapse = " ")
      }
    }
  }
  
  code <- paste0(code, "\n")
  
  ### Observation Probabilities
  
  ## if the observation probabilities are given in the general form
  if (is.data.frame(observation_prob)) {
    # checking if the number of the columns of the given data frame is 4
    if (dim(observation_prob)[2] != 4) {
      stop("the given data frame for the observation probabilities needs to have 4 columns including 'action', 'end-state','observation','probability'")
    }
     
    # writing the transition probabilities lines
    for (i in 1:dim(observation_prob)[1]) {
      # fix indexing
      if(is.numeric(observation_prob[i,1])) observation_prob[i,1] <- observation_prob[i,1] -1    
      if(is.numeric(observation_prob[i,2])) observation_prob[i,1] <- observation_prob[i,1] -1    
      if(is.numeric(observation_prob[i,3])) observation_prob[i,1] <- observation_prob[i,1] -1    
      
      code <- paste0(code,"O: ", 
                      observation_prob[i,1], " : ", 
                      observation_prob[i,2], " : ", 
                      observation_prob[i,3], " ", 
                      format(observation_prob[i,4], scientific = FALSE), "\n")
    }
  }else{
    ## if the observation probabilities are given in the form of action dependent matrices
    # checking if the number of the given observation probability matrices matches the number of actions
    if (length(observation_prob)!=number_of_actions) {
      stop("the number of given observation probability matrices does not match the number of actions")
    }
    # writing the observation probability matrices
    for (i in 1:number_of_actions) {
      code <- paste0(code,"O: ", actions[i], "\n")
      
      if (is.character(observation_prob[[actions[i]]]) && length(observation_prob[[actions[i]]]) == 1) {
        code <- paste0(code, observation_prob[[actions[i]]], "\n")
      } else {
        c_m <- character()
        for (j in 1:number_of_states) {
          c_m <- paste0(c_m, paste(format(observation_prob[[actions[i]]][j,], scientific = FALSE),
            collapse = " "), "\n")
        }
        code <- paste0(code, c_m)
      }
    }
  }
  
  code <- paste0(code, "\n")
  
  ### Rewards/Costs
  
  ## if the rewards are given in the general form
  if (is.data.frame(reward)) {
    # checking if the number of the columns of the given data frame is 5
    if (dim(reward)[2] != 5) {
      stop("the given data frame for the Rewards needs to have 5 columns including 'action', 'start-state','end-state','observation', 'values'")
    }
    
    # writing the reward lines
    for (i in 1:nrow(reward)) {
      code <- paste0(code,"R: ", 
                      format(reward[i,1], scientific = FALSE), " : ", 
                      format(reward[i,2], scientific = FALSE), " : ", 
                      format(reward[i,3], scientific = FALSE), " : ", 
                      format(reward[i,4], scientific = FALSE), " ",
                      format(reward[i,5], scientific = FALSE),  "\n")
    }
  }else{
    
    ## if the rewards are given in the form of action and start-state dependent matrices
    # checking if the number of the given reward matrices matches the number of actions and states
    if (length(reward)!= (number_of_actions)) {
      stop("the number of given list matrices does not match the number of actions")
    }
    for (i in 1:number_of_actions) {
      if (length(reward[[actions[i]]])!= (number_of_states)) {
        stop("the number of given reward matrices for action ", i ," does not match the number of states")
      }
    }
    # writing the reward matrices
    for (i in 1:number_of_actions) {
      for (j in 1:number_of_states) {
        code <- paste0(code,"R: ", actions[i], ":" , states[j], "\n")
        
        if (is.character(reward[[actions[i]]][[states[j]]]) && length(reward[[actions[i]]][[states[j]]]) == 1){
          code <- paste0(code, reward[[actions[i]]][[states[j]]] , "\n")
        } 
        else {
          c_m <- character()
          for (k in 1:number_of_states) {
            c_m <- paste0(c_m, paste(format(reward[[actions[i]]][[states[j]]][k,], scientific = FALSE),
              collapse = " "), "\n")
          }
          code <- paste0(code, c_m, "\n")
        }
      }
    }
  
    code <- paste0(code, "\n")
    
    # ## if the rewards are given in the form of action-and-start-state dependent matrix
    # # writing the reward section
    # for (i in 1:number_of_actions) {
    #   for (j in 1:number_of_states) {
    #     code <- paste(c(code, "R:" , actions[i], ":" , states[j], "\n"))
    #     c_m <- paste(rep(paste(c(rep(reward[i,j], number_of_observations), "\n"),
    #                            collapse = " "), number_of_states), collapse = " ")
    #     code <- paste(c(code, c_m),collapse = " ")
    #   }
    # }
    
    
  }
  
  ### saving the POMDP file
  cat(code, file = file)
}





