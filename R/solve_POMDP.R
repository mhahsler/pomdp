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
  discount = NULL,
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
    model <- structure(list(model = .parse_POMDP_model_file(model)), class = "POMDP")
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
    ifelse(!is.null(discount), paste("-discount", discount), ""),
    paras, 
    "-fg_save true")
 
  # fix discount if overwritten
  if(is.null(discount)) discount <- model$model$discount
  if(is.null(discount)) discount <- NA
   
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
    
  }
  
  # add solution to model
  model$solution <- structure(list(
    method = method, 
    parameter = parameter,
    horizon = horizon,
    discount = discount,
    converged = converged,
    total_expected_reward = NA,
    initial_pg_node = NA,
    belief_states = belief, 
    pg = pg,
    alpha = alpha
  ), class = "POMDP_solution")
  
  ## add initial node and reward 
  rew <- reward(model, belief = model$model$start)
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

## Helper functions

.parse_POMDP_model_file <- function(file) {
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
  colnames(alpha) <- model$model$states
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

# translate belief specifications into belief vectors
.translate_belief <- function(belief = NULL, model) {
  ## producing the starting belief vector
  
  if(is.null(belief)) belief <- "uniform"
  
  states <- model$model$states
  
  # start: 0.3 0.1 0.0 0.2 0.5
  if(is.numeric(belief) &&
      length(belief) == length(states) && 
      round(sum(belief), 3) == 1) {
    names(belief) <- states
    return(belief) 
  }
  
  # start: uniform
  if(is.character(belief) &&
      length(belief) == 1 && 
      belief[1] == "uniform") {
    belief <- rep(1/length(states), times = length(states))
    names(belief) <- states
    return(belief) 
  }
  
  
  # general checks for state IDs
  if(is.numeric(belief)) {
    belief <- as.integer(belief)
    if(any(abs(belief) <1) || any(abs(belief) > length(states)))
      stop("Illegal belief format.\n", belief,
        "\nState IDs need to be in [1, # of states].")
  }
  
  # general checks for state names
  else if(is.character(belief)) {
    if(any(is.na(match(belief, c(states, "-")))))
      stop("Illegal belief format.\n", belief,
        "\nUnrecognized state name.")
  
  } else stop("Illegal belief format.")
  
  #start: first-state
  #start: 5
  #start include: first-state third state
  #start include: 1 3
  if((is.numeric(belief) && all(belief > 0)) ||
      (is.character(belief) && belief[1] != "-")) {
    if(length(belief) > length(states)) 
      stop("Illegal belief format.\n", belief,
        "\nToo many states specified.")
    belief_ <- rep(0, times = length(states))
    names(belief_) <- states
    belief_[belief] <- 1/length(belief) 
    return(belief_)
  }
  
  #start exclude: 1 3
  if(is.numeric(belief) && any(belief < 0)) {
    belief_ <- rep(1, times = length(states))
    if(length(belief) >= length(states)) 
      stop("Illegal belief format.\n", belief,
        "\nToo many states specified.")
    names(belief_) <- states
    belief_[-belief] <- 0
    belief_ <- belief_/sum(belief_)
    return(belief_)
  }
  
  #start exclude: fifth-state seventh-state
  if(is.character(belief) && belief[1] == "-") {
    belief <- belief[-1]
    belief_ <- rep(1, times = length(states))
    names(belief_) <- states
    belief_[belief] <- 0
    belief_ <- belief_/sum(belief_)
    return(belief_)
  }
  
  stop("Illegal belief format.\n", belief)
}
  
