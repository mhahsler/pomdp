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
  terminal_values = NULL,
  method = "grid",
  parameter = NULL,
  verbose = FALSE) {

  if(is.null(horizon) || horizon < 1) horizon <- Inf 
  else horizon <- floor(horizon)
 
   
  converged <- NA
   
  methods <- c("grid", "enum", "twopass", "witness", "incprune") 
  # Not implemented:  "linsup", "mcgs"
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
  
  # discount
  if(is.null(discount)) discount <- model$model$discount
 
  # terminal values
  if(!is.null(terminal_values)) {
    if(!is.matrix(terminal_values)) terminal_values <- rbind(terminal_values)
    if(ncol(terminal_values) != length(model$model$states))
      stop("number of terminal values does not match the number of states.")
    colnames(terminal_values) <- as.character(model$model$states)
    
    terminal_values_filename <- .write_alpha_file(file_prefix, terminal_values)  
  }
   
  if(!is.null(parameter)) {
    paras <- sapply(names(parameter), FUN = function(n) 
      paste0("-", n, " ", if(is.logical(parameter[[n]])) { 
        if(parameter[[n]]) "true" else "false"} else parameter[[n]])
      )
  } else paras <- ""
  
 
  # for verbose it goes directly to the console ("") 
  # for finite horizon we need to save all pg and alpha files
  pomdp_args <- c(
    paste("-pomdp", pomdp_filename),
    paste("-method", method),
    ifelse(is.finite(horizon), paste("-horizon", horizon, "-save_all true"), ""),
    ifelse(!is.null(discount), paste("-discount", discount), ""),
    ifelse(!is.null(terminal_values), paste("-terminal_values", terminal_values_filename), ""),
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
  
  if(!is.null(attr(solver_output, "status")) || (verbose && solver_output !=0)) {
    if(!verbose) cat(paste(solver_output, "\n\n"))
    
    cat("Note that the action and state index reported by the solver starts with 0 and not with 1:\n")
    
    m <- max(length(model$model$states), 
      length(model$model$actions),
      length(model$model$observations)
    )
    
    print(data.frame(index = (1:m)-1, 
      action = model$model$actions[1:m],
      state = model$model$states[1:m], 
      observation = model$model$observations[1:m]
    ))
    
    cat("\n")
    
    stop("POMDP solver returned an error (see above).")
  }
  
  ## converged infinite horizon POMDPs produce a policy graph 
  if(!is.finite(horizon)) { 
    converged <- TRUE
    alpha <- .get_alpha_file(file_prefix, model)
    pg <- .get_pg_file(file_prefix, model)
    belief <- .get_belief_file(file_prefix, model)
    
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
    
    if(!converged && method == "grid") warning("The grid method for finite horizon did not converge. The reward values may not be valid if there are negative rewards.")
    
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
    initial_belief = NA,
    initial_pg_node = NA,
    terminal_values = terminal_values, 
    belief_states = belief, 
    pg = pg,
    alpha = alpha
  ), class = "POMDP_solution")
  
  ## add initial node and reward 
  rew <- reward(model, belief = model$model$start)
  model$solution$initial_belief <- rew$belief
  model$solution$total_expected_reward <- rew$reward
  model$solution$initial_pg_node <- rew$pg_node
  
  model$solver_output <- structure(solver_output, class = "text")
   
  model
}



print.POMDP_solution <- function(x, ...) {
 cat("POMDP solution\n\n")
 print(unclass(x))
}

