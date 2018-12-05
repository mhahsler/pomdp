library(igraph)

pomdp <- function(discount = 0,
                  states,
                  actions,
                  observations,
                  start = "uniform",
                  transition_prob,
                  observation_prob,
                  reward,
                  values = "reward",
                  grid_size,
                  verbose = FALSE) {
  
  # discount shuold be a number in [0,1]
  # states should be a vector of strings
  # actions should be a vector of strings
  # observations should be a vector of strings
  # start should be either a vector of n numbers each in [0,1] that add up to 1 where n is the number of states
  # or the word "uniform", or a single number in 1 to n, or the name of a single state, or the names of a subset of states
  # transition_prob is either a list consisting of m matrices where m is the number of actions
  # or a data frame with 4 columns
  # observation_prob is either a list consisting of m matrices where m is the number of actions 
  # or a data frame with 4 columns
  # reward should be either a matrix of size mxn where n is the number of states or 
  # a data frame with 5 columns
  # grid_size is an integer
  
  ### model
  model <- list(discount = discount, states = states, actions = actions, 
    observations = observations, start = start, transition_prob = transition_prob,
    observation_prob = observation_prob, reward = reward)
  
  
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
  pomdp_file <- tempfile(pattern = "model", fileext = ".POMDP")
  cat(code, file = pomdp_file)
  
  ### running the POMDP code
  exec <- system.file("pomdp-solve", package="pomdp")
  shell_command <- sprintf("%s -pomdp %s -method grid -fg_points %d -fg_save true",
    exec, pomdp_file, grid_size)
  solver_output <- system(shell_command, intern=TRUE,
    ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE)
  
  ## the verbose mode: printing all the outputs from pomdp solver
  if(verbose) cat(paste(solver_output, "\n"))
  
  ### importing the outputs and results
  file_id <- as.numeric(substr(solver_output[5], 11, 15))
  result_files <- gsub(".POMDP", "", pomdp_file)
  
  ## Creating result files' names and extensions
  pg_filename <- sprintf("%s-%d.pg", result_files, file_id)
  belief_filename <- sprintf("%s-%d.belief", result_files, file_id)
  alpha_filename <- sprintf("%s-%d.alpha", result_files, file_id)
  
  ## importing pg file
  pg <- read.table(pg_filename, header = FALSE, sep = " ", quote = "\"", 
    dec = ".", na.strings = NA, numerals = "no.loss")
  pg <- as.matrix(pg)
  pg <- pg[,c(-3,-dim(pg)[2])] #there 2 NA columns that need to be removed
  pg <- pg+1 #index has to start from 1 not 0
  pg_matrix <- pg
  pg <- as.data.frame(pg)
  if (dim(pg)[2]==1 ) {
    pg <- t(pg)
  }
  
  ## importing belief file
  belief <- read.table(belief_filename) 
  belief_matrix <- as.matrix(belief)
  belief <- as.data.frame(belief_matrix)
  
  ## importing alpha file
  alpha <- read.table(alpha_filename, header = FALSE, sep = "\n")
  alpha <- as.matrix(alpha)
  toDel <- seq(1,dim(alpha)[1],2)
  alpha <- alpha[-toDel,]
  alpha <- unlist(strsplit(alpha, " "))
  alpha<- matrix(as.numeric(alpha), ncol = number_of_states, byrow = TRUE)
  alpha_matrix <- alpha
  alpha <- as.data.frame(alpha)
  
  
  ## modifying the pg file
  # renamig the columns
  colnames(pg)[1] <- "belief"
  colnames(pg)[2] <- "action"
  for (i in 1:number_of_observations) {
    colnames(pg)[i+2] <- observations[i]
  }
  # renaming the actions
  for (i in 1:number_of_actions) {
    pg[pg[,2]==i,2] <- actions[i]
  }
  
  ## modifying the alpha file
  # renaming the columns
  for (i in 1:number_of_states) {
    colnames(alpha)[i] <- paste("coeffecient", i)
  }
  
  ## modifying the belief file
  # renaming the columns
  for (i in 1:number_of_states) {
    colnames(belief)[i] <- states[i]
  }
  
  
  ### outputs and results
  
  ## producing the starting belief vector
  if (!is.character(start)) {
    if (sum(start)==1) {
      start_belief <- start
    }
  }
  # if the starting beliefs are given by a uniform distribution over all states
  if (sum(start== "uniform")==1) {
    start_belief <- rep(1/number_of_states,number_of_states)
  } else if (start[1]!="-") {  # if the starting beliefs include a specific subset of states
    # if the starting beliefs are given by a uniform distribution over a subset of states (using their names)
    if (!is.na(sum(match(start , states)))) {
      start_belief <- rep(0,number_of_states)
      start_belief[match(start,states)] <- 1/length(start)
    }
    # if the starting beliefs are given by a uniform distribution over a subset of states (using their numbers)
    if (!is.character(start)) { 
      if (sum(start)>=1 & length(start)<number_of_states) {
        start_belief <- rep(0,number_of_states)
        start_belief[start] <- 1/length(start)
      }
    }
  } else if (start[1]=="-") { # if the starting beliefs exclude a specific subset of states
    start_belief <- rep(1/(number_of_states-length(start)+1),number_of_states)
    if (is.na(as.numeric(start[2]))) {
      start_belief[match(start,states)] <- 0
    }
    if (!is.na(as.numeric(start[2]))) {
      start_belief[start] <- 0
    }
  }
  ## calculating the total expected reward
  initial_node <- which.max(alpha_matrix %*% start_belief)
  total_expected_reward <- max(alpha_matrix %*% start_belief)
  
  ## finding the respective proportions for each line (node)
  for (i in 1:dim(belief_matrix)[1]) {
    belief[i,number_of_states+1]<- which.max(alpha_matrix %*% belief_matrix[i,])
  }
  colnames(belief)[number_of_states+1] <- "line"
  
  belief_proportions <- alpha-alpha
  colnames(belief_proportions) <- colnames(belief)[1:number_of_states]
  
  for (i in 1:dim(alpha_matrix)[1]) {
    c <- 0
    for (j in 1: dim(belief_matrix)[1]) {
      if (belief[j,ncol(belief)]==i) {
        c <-  c + 1
        belief_proportions[i,] <- belief_proportions[i,]+belief_matrix[j,]
      }
    }
    belief_proportions[i,] <- belief_proportions[i,]/c
  }
  
  
  structure(list(belief = belief, 
                 belief_proportions = belief_proportions, 
                 alpha = alpha, 
                 pg = pg,
                 total_expected_reward = total_expected_reward,
                 initial_node = initial_node,
                 solver_output = solver_output,
                 model = model),
            class = "POMDP")
}

print.POMDP <- function(x, ...) {
  cat("POMDP Object with attributes including belief, belief proportions, alpha, pg, total expected reward, initial node, solver output and the model")
}



