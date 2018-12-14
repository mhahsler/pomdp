
#solve a POMDP model
solve_POMDP <- function(
  model,
  horizon = NULL,
  method = "grid",
  parameter = NULL,
  verbose = FALSE) {

  ### DEBUG
  #verbose <- TRUE
  ###
    
  methods <- c("grid", "enum", "twopass", "witness", "incprune")
  ### Not implemented:  "linsup", "mcgs"
  method <- match.arg(method, methods)
  
  ### write model to file
  file_prefix <- tempfile(pattern = "model")
  pomdp_filename <- paste0(file_prefix, ".POMDP") 
  write_POMDP(model, pomdp_filename)
    
  ### running the POMDP code
  exec <- system.file(c("pomdp-solve", "pomdp-solve.exe"), package="pomdp")
  if(exec[1] == "") stop("pomdp-solve executable not found. Reinstall package pomdp.")
  
  if(!is.null(parameter)) {
    paras <- sapply(names(parameter), FUN = function(n) paste0("-", n, " ", parameter[[n]]))
  } else paras <- ""
  
  solver_output <- system2(exec[1], 
    args = c(paste("-pomdp", pomdp_filename),
      paste("-method", method),
      (if(!is.null(horizon)) paste("-horizon", horizon) else ""),
      paras, 
      "-fg_save true"),
    stdout = TRUE, stderr = TRUE, wait = TRUE
  )
    
  ## the verbose mode: printing all the outputs from pomdp solver
  if(verbose) cat(paste(solver_output, "\n"))
  
  ### importing the outputs and results (pomdp-solve adds the PID to the file prefix)
  file_prefix <- gsub("^o = (.*)\\s*$", "\\1", solver_output[16]) 
  
  ## Creating result files' names and extensions
  pomdp_filename <- paste0(file_prefix, ".POMDP") 
  pg_filename <- paste0(file_prefix, ".pg")
  belief_filename <- paste0(file_prefix, ".belief")
  alpha_filename <- paste0(file_prefix, ".alpha")
  
  ## importing alpha file
  number_of_states <- length(model$states)
  number_of_observations <- length(model$observations)
  number_of_actions <- length(model$actions)
  observations <- model$observations
  actions <- model$actions
  states <- model$states
  start <- model$start
  
  alpha <- read.table(alpha_filename, header = FALSE, sep = "\n")
  alpha <- as.matrix(alpha)
  toDel <- seq(1,dim(alpha)[1],2)
  alpha <- alpha[-toDel,]
  alpha <- unlist(strsplit(alpha, " "))
  alpha<- matrix(as.numeric(alpha), ncol = number_of_states, byrow = TRUE)
  alpha_matrix <- alpha
  alpha <- as.data.frame(alpha)
  
  # renaming the columns
  for (i in 1:number_of_states) {
    colnames(alpha)[i] <- paste("coeffecient", i)
  }
  
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
  
  ## importing belief file if it exists
  if(file.exists(belief_filename)) {
    belief <- read.table(belief_filename) 
    belief_matrix <- as.matrix(belief)
    belief <- as.data.frame(belief_matrix)
    
    # renaming the columns
    for (i in 1:number_of_states) {
      colnames(belief)[i] <- states[i]
    }
  
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
  } else {
    belief <- NULL
    belief_proportions <- NULL
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
  initial_belief_state <- which.max(alpha_matrix %*% start_belief)
  total_expected_reward <- max(alpha_matrix %*% start_belief)
  
  solution <- structure(list(
    method = method, 
    parameter = parameter,
    alpha = alpha,
    pg = pg,
    belief = belief, 
    belief_proportions = belief_proportions,
    total_expected_reward = total_expected_reward,
    initial_belief_state = initial_belief_state
  ), class = "POMDP_solution")
  
  structure(list(model = model,
    solution = solution,
    solver_output = solver_output
  ), class = "POMDP")
}

print.POMDP <- function(x, ...) {
  cat(
    "Solved POMDP model:", x$model$name, "\n", 
    "\tmethod:", x$solution$method, "\n",
    "\tbelief states:", nrow(x$solution$pg), "\n",
    paste0("\ttotal expected ", x$model$values, ":"), 
      x$solution$total_expected_reward,"\n\n" 
  )
}

print.POMDP_solution <- function(x, ...) {
 cat("POMDP solution\n\n")
 print(unclass(x))
}