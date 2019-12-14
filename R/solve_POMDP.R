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
  horizon = Inf,
  method = "grid",
  parameter = NULL,
  verbose = FALSE) {

  ### DEBUG
  #verbose <- TRUE
  ###
  
  if(is.null(horizon) || horizon < 1) horizon <- Inf 
  else horizon <- floor(horizon)
  
  converged <- NA
   
  methods <- c("grid", "enum", "twopass", "witness", "incprune") # Not implemented:  "linsup", "mcgs"
  method <- match.arg(method, methods)
    
  ### write model to file
  file_prefix <- tempfile(pattern = "pomdp_")
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
  
 
  # for verbose it goes directly to the console ("") 
  # for finite horizon we need to save all pg and alpha files
  pomdp_args <- c(
    paste("-pomdp", pomdp_filename),
    paste("-method", method),
    ifelse(is.finite(horizon), paste("-horizon", horizon, "-save_all true"), ""),
    paras, 
    "-fg_save true")
  
  if(verbose) cat("Calling pomdp-solve with the following arguments:", 
    paste(pomdp_args, collapse =  " "), 
    "\nSolver output:", sep = "\n")
  
  solver_output <- system2(find_pomdpsolve(), args = pomdp_args,
    stdout = ifelse(verbose, "", TRUE), stderr = ifelse(verbose, "", TRUE), wait = TRUE
  )
  
  if(!is.null(attr(solver_output, "status"))) {
    cat(paste(solver_output, "\n"))
    stop("POMDP solver returned an error. Note that the action and state index for the solver starts with 0 and not with 1. So action=0 is the first action.")
  }
  
  ## converged infinite horizon POMDPs produce a policy graph 
  if(!is.finite(horizon)) { 
    converged <- TRUE
    alpha <- .get_alpha_file(file_prefix, model)
    pg <- .get_pg_file(file_prefix, model)
    belief <- .get_belief_file(file_prefix, model)
    
    ## finding the respective proportions for each line (node)
    if(!is.null(belief)) {
      belief <- cbind(belief, node = apply(belief, MARGIN = 1, FUN = function(b) which.max(alpha %*% b)))
      
      belief_proportions <- t(sapply(1:nrow(pg), FUN = 
          function(i) colMeans(belief[belief[,"node"] == i, -ncol(belief), drop = FALSE])))
    } else {
      belief_proportions <- NULL
    }
    
  }else{
    ## finite horizon pomdp: read the policy tree
    belief <- .get_belief_file(file_prefix, model)
    
    converged <- FALSE ### did the grid method converge?
    alpha <- list()
    pg <- list()
    for(i in 1:horizon) {
      ## no more files exist after convergence 
      r <- suppressWarnings(try({
        alpha[[i]] <- .get_alpha_file(file_prefix, model, i)
        pg[[i]] <- .get_pg_file(file_prefix, model, i)
      }, silent = TRUE))
      if(inherits(r, "try-error")) {
        if(verbose) cat("Convergence: Finite-horizon POMDP converged early at epoch:", i-1, "\n")
        converged <- i-1
        break
      }
    }
    
    if(!converged && method == "grid") warning("The grid method for finite horizon did not converge. The reward values may not be valid. Increase the horizon ot use an alternative method.")
    
    alpha <- rev(alpha)
    pg <- rev(pg)
    
    belief_proportions <- NULL
  }
  
  # add solution to model
  model$solution <- structure(list(
    method = method, 
    parameter = parameter,
    horizon = horizon,
    converged = converged,
    total_expected_reward = NA,
    initial_pg_node = NA,
    belief_states = belief, 
    pg = pg,
    alpha = alpha,
    belief_proportions = belief_proportions
  ), class = "POMDP_solution")
  
  ## add initial node and reward 
  rew <- reward(model, start = model$model$start)
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
  .solved_POMDP(x)
  
  cat(x$solver_output, sep = "\n")
  invisible(x$solver_output)
}

reward <- function(x, start = "uniform") {
  .solved_POMDP(x)
  
  states <- x$model$states
  number_of_states <- length(states)
 
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
 
  ## alpha and pg is a list for finite horizon POMDPS
  if(is.list(x$solution$alpha)) alpha <- x$solution$alpha[[1]]
  else alpha <- x$solution$alpha
   
  initial_pg_node <- which.max(alpha %*% start_belief)
  total_expected_reward <- max(alpha %*% start_belief)
  
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

  

.get_alpha_file <- function(file_prefix, model, number = "") {  
  filename <- paste0(file_prefix, '-0.alpha',number)
  ## importing alpha file
  alpha <- readLines(filename)
  alpha <- alpha[seq(2, length(alpha), 3)]
  alpha <- do.call(rbind, lapply(alpha, function(a) as.numeric(strsplit(a, " ")[[1]])))
  colnames(alpha) <- paste0("coef_", 1:ncol(alpha))
  alpha
}

## helpers to read pomdp-solve files

## importing pg file
.get_pg_file <- function(file_prefix, model, number="") {
  filename <- paste0(file_prefix,'-0.pg', number)
  pg <- read.table(filename, header = FALSE, sep = "", 
    colClasses = "numeric", na.strings = "-")
  pg <- pg + 1 #index has to start from 1 not 0
  
  ### FIXME: I am not sure we need this now
  #if (dim(pg)[2]==1 ) {
  #  pg <- t(pg)
  #}
  
  # renaming the columns and actions
  colnames(pg) <- c("node", "action", model$model$observations)
  pg[,2] <- model$model$actions[pg[,2]]
  pg
}
  
## importing belief file if it exists
.get_belief_file <- function(file_prefix, model) {
  filename <- paste0(file_prefix,'-0.belief')
  if(!file.exists(filename)) return(NULL)
  
  belief <- as.matrix(read.table(filename)) 
  colnames(belief) <- model$model$states
  belief
} 

