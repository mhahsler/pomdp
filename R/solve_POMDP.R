# Class POMDP is a list with model and solution.
# solve_POMDP uses the model and adds the solution to the list.

find_pomdpsolve <- function() {
  exec <- system.file(c("pomdp-solve", "pomdp-solve.exe"), package="pomdp")[1]
  if(exec == "") stop("pomdp-solve executable not found. Reinstall package pomdp.")
  exec
}
  
solve_POMDP_parameter <- function() {
  solver_output <- system2(find_pomdpsolve(), 
    args = c("-h"),
    stdout = TRUE, stderr = TRUE, wait = TRUE
  )
    
  cat(solver_output, sep = "\n")
  cat("\nUse the parameter options in solve_POMDP without the leading '-' in the form:",
    "\tparameter = list(fg_points = 100)",
    "Note: Not all parameter options are available (e.g., resource limitations, -pomdp, -horizon).", sep = "\n")
}


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
  
  methods <- c("grid", "enum", "twopass", "witness", "incprune") # Not implemented:  "linsup", "mcgs"
  method <- match.arg(method, methods)
  
  ### write model to file
  file_prefix <- tempfile(pattern = "model")
  pomdp_filename <- paste0(file_prefix, ".POMDP") 
  
  # is model a filename or a URL?
  if(is.character(model)) {
    model <- structure(list(model = parses_POMDP_model_file(model)), class = "POMDP")
    writeLines(model$model$problem, con = pomdp_filename)
  } else {
    write_POMDP(model, pomdp_filename)
  }
  
  if(!is.null(parameter)) {
    paras <- sapply(names(parameter), FUN = function(n) paste0("-", n, " ", parameter[[n]]))
  } else paras <- ""
  
  solver_output <- system2(find_pomdpsolve(), 
    args = c(paste("-pomdp", pomdp_filename),
      paste("-method", method),
      (if(!is.null(horizon)) paste("-horizon", horizon) else ""),
      paras, 
      "-fg_save true"),
    stdout = TRUE, stderr = TRUE, wait = TRUE
  )
   
  if(!is.null(attr(solver_output, "status"))) {
    cat(paste(solver_output, "\n"))
    stop("POMDP solver returned an error. Note that the action and state index for the solver starts with 0 and not with 1. So action=0 is the first action.")
  }
   
  ## the verbose mode: printing all the outputs from pomdp solver
  if(verbose) cat(paste(solver_output, "\n"))
  
  ### importing the outputs and results (pomdp-solve adds the PID to the file prefix)
  file_prefix <- gsub("^o = (.*)\\s*$", "\\1", solver_output[16]) 
  
  ## Creating result files' names and extensions
  pomdp_filename <- paste0(file_prefix, ".POMDP") 
  pg_filename <- paste0(file_prefix, ".pg")
  belief_filename <- paste0(file_prefix, ".belief")
  alpha_filename <- paste0(file_prefix, ".alpha")
  
  states <- model$model$states
  actions <- model$model$actions
  observations <- model$model$observations
  start <- model$model$start
  
  ## importing alpha file
  alpha <- readLines(alpha_filename)
  alpha <- alpha[seq(2, length(alpha), 3)]
  alpha <- do.call(rbind, lapply(alpha, function(a) as.numeric(strsplit(a, " ")[[1]])))
  colnames(alpha) <- paste0("coef_", 1:ncol(alpha))
  
  number_of_states <- ncol(alpha)
  
  ## importing pg file
  pg <- read.table(pg_filename, header = FALSE, sep = "", colClasses = "numeric", na.strings = "-")
  pg <- pg + 1 #index has to start from 1 not 0
  
  ### FIXME: I am not sure we need this now
  #if (dim(pg)[2]==1 ) {
  #  pg <- t(pg)
  #}
  
  # renaming the columns and actions
  colnames(pg) <- c("node", "action", observations)
  pg[,2] <- actions[pg[,2]]
  
  ## importing belief file if it exists
  if(file.exists(belief_filename)) {
    belief <- as.matrix(read.table(belief_filename)) 
    colnames(belief) <- states
  
    ## finding the respective proportions for each line (node)
    belief <- cbind(belief, node = apply(belief, MARGIN = 1, FUN = function(b) which.max(alpha %*% b)))
    
    belief_proportions <- t(sapply(1:nrow(pg), FUN = 
        function(i) colMeans(belief[belief[,"node"] == i, -ncol(belief), drop = FALSE])))
    
  } else {
    belief <- NULL
    belief_proportions <- NULL
  }
  
  # add solution to model
  model$solution <- structure(list(
    method = method, 
    parameter = parameter,
    total_expected_reward = NA,
    initial_pg_node = NA,
    belief_states = belief, 
    pg = pg,
    alpha = alpha,
    belief_proportions = belief_proportions
  ), class = "POMDP_solution")
   
  
  ## add initial node and reward 
  rew <- reward(model, start = start)
  model$solution$total_expected_reward <- rew$total_expected_reward
  model$solution$initial_pg_node <- rew$initial_pg_node
 
  model$solver_output <- solver_output
   
  model
}

print.POMDP_solution <- function(x, ...) {
 cat("POMDP solution\n\n")
 print(unclass(x))
}

solver_output <- function(x) {
  if(!inherits(x, "POMDP")) stop("x needs to be a POMDP object!")
  if(is.null(solution(x))) stop("x conatains an unsolved POMDP.")
  cat(x$solver_output, sep = "\n")
  invisible(x$solver_output)
}

reward <- function(x, start = "uniform") {
  if(!inherits(x, "POMDP")) stop("reward requires a solved POMDP problem, i.e., an object of class POMDP.")
  
  if(is.null(start)) return(list(total_expected_reward = NA, initial_pg_node = NA))
  
  number_of_states <- ncol(x$solution$alpha)
  states <- x$model$states
 
  ## FIXME: This needs fixing!   
  ## producing the starting belief vector
  if (!is.character(start) && length(start) == number_of_states && round(sum(start), 3) == 1) {
    # a vector with probabilities
    start_belief <- start
  } else if (length(start) == 1 && start[1] == "uniform") {
    # if the starting beliefs are given by a uniform distribution over all states
    start_belief <- rep(1/number_of_states, number_of_states)
  } else if (start[1] != "-") {  # if the starting beliefs include a specific subset of states
    # if the starting beliefs are given by a uniform distribution over a subset of states (using their names)
    if (!is.na(sum(match(start, states)))) {
      start_belief <- rep(0, number_of_states)
      start_belief[match(start, states)] <- 1/length(start)
    }
    # if the starting beliefs are given by a uniform distribution over a subset of states (using their numbers)
    if (!is.character(start)) { 
      if (all(start >= 1 & start <= number_of_states & start == floor(start))) {
        start_belief <- rep(0,number_of_states)
        start_belief[start] <- 1/length(start)
      } else stop("illegal start belief state specification.")
    }
  } else if (start[1]=="-") { # if the starting beliefs exclude a specific subset of states
    start_belief <- rep(1/(number_of_states-length(start)+1),number_of_states)
    if (is.na(as.numeric(start[2]))) {
      start_belief[match(start,states)] <- 0
    }
    if (!is.na(as.numeric(start[2]))) {
      start_belief[start] <- 0
    }
  } else stop("illegal start belief state specification.")
  
  names(start_belief) <- states
  
  initial_pg_node <- which.max(x$solution$alpha %*% start_belief)
  
  
  total_expected_reward <- max(x$solution$alpha %*% start_belief)
  
  list(total_expected_reward = total_expected_reward, initial_pg_node = initial_pg_node,
    start_belief_state = start_belief)
}


parses_POMDP_model_file <- function(file) {
    problem <- readLines(file)  
    
    get_vals <- function(var) {
      ind <- grep(paste0(var,":"), problem)
      if(length(ind) == 0) return(NULL)
      
      vals <- strsplit(problem[[ind]], "\\s+")[[1]][-1]
      
      # the data may be in the next line
      if(length(vals) == 0) vals <- strsplit(problem[[ind+1]], "\\s+")[[1]]
      
      # numbers?
      vals <- type.convert(vals, as.is = TRUE)
      
      # create labels if just the number is mentioned
      if(length(vals) == 1 && is.numeric(vals)) 
        vals <- paste0(substr(var, 1, 1), seq(vals)) 
    vals
    }
    
    structure(list(
      name = file,
      states = get_vals("states"),
      observations = get_vals("observations"),
      actions = get_vals("actions"),
      start = get_vals("start"),
      problem = problem),
      class = "POMDP_model"
    )
}    
    
