# Class POMDP is a list with model and solution.
# solve_POMDP uses the model and adds the solution to the list.

find_pomdpsolve <- function() {
  exec <- system.file(file.path("bin", .Platform$r_arch, c("pomdp-solve", "pomdp-solve.exe")), package="pomdp")[1] 
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
  discount = NULL,
  terminal_values = NULL,
  method = "grid",
  digits = 7,
  parameter = NULL,
  verbose = FALSE) {
  
  converged <- NA
   
  methods <- c("grid", "enum", "twopass", "witness", "incprune") 
  # Not available (linsup need CPLEX and mcgs is not finished):  "linsup", "mcgs")
  method <- match.arg(method, methods)
  
  # do we have a model POMDP file?
  if(is.character(model)) 
    model <- structure(list(model = read_POMDP(model)), class = "POMDP")
  
  if(is.null(horizon)) horizon <- model$model$horizon
  
  # time-dependent POMDP?
  if(.timedependent_POMDP(model)) 
    return(.solve_POMDP_time_dependent(
      model = model, horizon = horizon, discount = discount,
      terminal_values = terminal_values, method = method,
      parameter = parameter, verbose = verbose))
  if(is.null(horizon) || horizon < 1) horizon <- Inf 
  else horizon <- floor(horizon)
  
  if(is.null(terminal_values)) terminal_values <- model$model$terminal_values
  if(!is.null(terminal_values) && 
      length(terminal_values) == 1 && terminal_values == 0) terminal_values <- NULL
  
  if(is.null(discount)) discount <- model$model$discount
  if(is.null(discount)) {
    warning("No discount rate specified. Using .9!")
    discount <- .9
  }
  
  ### temp file names
  file_prefix <- tempfile(pattern = "pomdp_")
  pomdp_filename <- paste0(file_prefix, ".POMDP") 
  
  # write model POMDP file
  if(!is.null(model$model$problem)) 
    writeLines(model$model$problem, con = pomdp_filename)
  else
    write_POMDP(model, pomdp_filename, digits = digits)
  
  # write terminal values file
  if(!is.null(terminal_values)) {
    if(!is.matrix(terminal_values)) terminal_values <- rbind(terminal_values)
    if(ncol(terminal_values) != length(model$model$states))
      stop("number of terminal values does not match the number of states.")
    colnames(terminal_values) <- as.character(model$model$states)
    
    terminal_values_filename <- .write_alpha_file(file_prefix, terminal_values)  
  }
  
  # write grid file for method grid
  if(!is.null(parameter$grid)) {
    if(method != "grid") warning("Custom grids are ignored by all methods but 'grid'!")
  
    # TODO: check grid 
    parameter$grid_filename <- .write_grid_file(file_prefix, parameter$grid)
    parameter$grid <- NULL
    parameter$fg_type <- "file"
  }
   
  # construct parameter string
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
    alpha <- list(.get_alpha_file(file_prefix, model))
    pg <- list(.get_pg_file(file_prefix, model))
    
  }else{
    ## finite horizon pomdp: read the policy tree
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
    
    
    if(method == "grid" && 
        !converged && 
        any(unlist(reward_matrix(model))<0)) 
      warning("The grid method for finite horizon did not converge. The value function and the calculated reward values may not be valid with negative reward in the reward matrix.")
    
    alpha <- rev(alpha)
    pg <- rev(pg)
    
  }
   
  # read belief states if available (method: grid) 
  belief <- .get_belief_file(file_prefix, model)
  
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
    terminal_values = if(!is.null(terminal_values)) terminal_values else 0, 
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


# is a field time dependent? For time-dependence we have a list of 
# matrices/data.frames or for observation_prob we have a list of a list
.is_timedependent <- function(x, field) {
  m <- x$model[[field]]
  
  if(is.null(m)) stop("Field ", field, " does not exist.")
  
  # for observation_prob
  if(field == "observation_prob") m <- m[[1]]
  
  if(!is.list(m) || is.data.frame(m)) return(FALSE)
  
  if(length(m) != length(x$model$horizon))
    stop("Inconsistent POMDP specification. Field ", field,
      " does not contain data for the appropriate number of episodes.")
  
  TRUE
}

# solve time-dependent POMDP
# we can have different transition_probs, observation_probs or rewards
.solve_POMDP_time_dependent <- function(model, horizon = NULL, ..., terminal_values = NULL, verbose = FALSE) {
    
  if(verbose) cat("\n+++++++++ time-dependent POMDP +++++++++\n", sep = "")
  
  if(is.null(horizon)) horizon <- model$model$horizon
  n <- length(horizon)
  if(n < 2) return(solve_POMDP(model, horizon, ..., 
    terminal_values = terminal_values, verbose = verbose))
  
  # check what is time dependent
  do_trans <- .is_timedependent(model, "transition_prob")
  do_obs <- .is_timedependent(model, "observation_prob")
  do_reward <- .is_timedependent(model, "reward")
  
  if(verbose) { 
    if(do_trans) cat(" * Using time-dependent transition probabilities.\n")
    if(do_obs) cat(" * Using time-dependent observation probabilities.\n")
    if(do_reward) cat(" * Using time-dependent rewards.\n")
  }
  
  # solve POMDPS in reverse order
  s <- list()
  m <- model
  if(!is.null(terminal_values)) m$model$terminal_values <- terminal_values
  
  for(i in n:1) {
    if(i < n) m$model$terminal_values <- prev_alpha
    m$model$horizon <- model$model$horizon[i]
    
    # match names instead of index?
    if(is.null(names(m$model$horizon))) take <- i
    else take <- names(m$model$horizon)
    
    if(verbose) cat(
      "\n++++++++++++++++++++++++++++++++++++++++\n", 
      "Solving episode ", i, " of ", n, " (", take, ") with horizon ", m$model$horizon, 
      "\n", sep = "")
   
    
    if(do_trans) m$model$transition_prob <- model$model$transition_prob[[take]]
    if(do_obs) m$model$observation_prob <- model$model$observation_prob[[take]]
    if(do_reward) m$model$reward <- model$model$reward[[take]]
   
    if(verbose) {
      if(do_trans) {
        cat("Using transition probabilities:\n") 
        print(m$model$transition_prob)
        }
    }
    
    s[[i]] <- solve_POMDP(m, ..., verbose = verbose)
    prev_alpha <- s[[i]]$solution$alpha[[1]]
  }
  
  # combine the results
  pgs <- unlist(lapply(s, FUN = function(x) x$solution$pg), recursive = FALSE)
  alphas <- unlist(lapply(s, FUN = function(x) x$solution$alpha), recursive = FALSE)
  reward <- sum(sapply(s, FUN = function(x) x$solution$total_expected_reward))
  
  m <- structure(list(model = model$model, solution = s[[1]]$solution), class = "POMDP")
  m$solution$pg <- pgs
  m$solution$alpha <- alphas
  m$solution$horizon <- sum(model$model$horizon)
  
  m
}
