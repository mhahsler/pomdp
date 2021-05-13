# Class POMDP is a list with model and solution.
# solve_POMDP uses the model and adds the solution to the list.

find_pomdpsolve <- function() {
  exec <- system.file(file.path("bin", .Platform$r_arch, c("pomdp-solve", "pomdp-solve.exe")), package="pomdp")[1] 
  if(exec == "") stop("pomdp-solve executable not found. Reinstall package pomdp.")
  exec
}

#' @rdname solve_POMDP
#' @export  
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


#' Solve a POMDP Problem using pomdp-solver
#' 
#' This function utilizes the C implementation of 'pomdp-solve' by Cassandra
#' (2015) to solve problems that are formulated as partially observable Markov
#' decision processes (POMDPs). The result is an optimal or approximately
#' optimal policy.
#' 
#' \code{solve_POMDP_parameter()} displays available solver parameter options.
#' 
#' \bold{Horizon:} Infinite-horizon POMDPs (\code{horizon = Inf}) converge to a
#' single policy graph. Finite-horizon POMDPs result in a policy tree of a
#' depth equal to the smaller of the horizon or the number of epochs to
#' convergence.  The policy (and the associated value function) are stored in a
#' list by epoch. The policy for the first epoch is stored as the first
#' element.
#' 
#' \bold{Policy:} Each policy is a data frame where each row representing a
#' policy graph node with an associated optimal action and a list of node IDs
#' to go to depending on the observation (specified as the column names). For
#' the finite-horizon case, the observation specific node IDs refer to nodes in
#' the next epoch creating a policy tree.  Impossible observations have a
#' \code{NA} as the next state.
#' 
#' \bold{Value function:} The value function is stored as a matrix. Each row is
#' associated with a node (row) in the policy graph and represents the
#' coefficients (alpha vector) of a hyperplane. An alpha vector contains one
#' value per state and is the value for the belief state that has a probability
#' of 1 for that state and 0s for all others.
#' 
#' \bold{Precision:} The POMDP solver uses various epsilon values to control
#' precision for comparing alpha vectors to check for convergence, and solving
#' LPs. Overall precision can be changed using \code{parameter = list(epsilon =
#' 1e-3)}.
#' 
#' \bold{Methods:} Several algorithms for dynamic-programming updates are
#' available: 
#' \itemize{ 
#' \item Enumeration (Sondik 1971). 
#' \item Two pass (Sondik 1971). 
#' %exhaustive (Monahan 1982), 
#' %linear support (Cheng 1988), 
#' \item
#' Witness (Littman, Cassandra, Kaelbling, 1996). 
#' \item Incremental pruning (Zhang and Liu, 1996, Cassandra et al 1997). 
#' \item Grid implements a variation of point-based value iteration 
#'   to solve larger POMDPs (PBVI; see Pineau 2003) without dynamic belief set expansion. 
#' } 
#' Details can be found in (Cassandra, 2015).
#' 
#' \bold{Note on method grid:} The grid method implements a version of Point
#' Based Value Iteration (PBVI). The used belief points are by default created
#' using points that are reachable from the initial belief (\code{start}) by
#' following all combinations of actions and observations. The size of the grid
#' can be set via \code{parameter = list(fg_points = 100)}. Alternatively,
#' different strategies can be chosen using the parameter \code{fg_type}. In
#' this implementation, the user can also specify manually a grid of belief
#' states by providing a matrix with belief states as produced by
#' \code{sample_belief_space} as the parameter \code{grid}.
#' 
#' To guarantee convergence in point-based (finite grid) value iteration, the
#' initial value function must be a lower bound on the optimal value function.
#' If all rewards are strictly non-negative, an initial value function with an
#' all zero vector can be used and results will be similar to other methods.
#' However, if there are negative rewards, lower bounds can be guaranteed by
#' setting a single vector with the values \eqn{min(reward)/(1 - discount)}.
#' The value function is guaranteed to converge to the true value function, but
#' finite-horizon value functions will not be as expected. \code{solve_POMDP}
#' produces a warning in this case.
#' 
#' \bold{Time-dependent POMDPs:} Time dependence of transition probabilities,
#' observation probabilities and reward structure can be modeled by considering
#' a set of episodes representing epoch with the same settings. In the scared
#' tiger example (see Examples section), the tiger has the normal behavior for
#' the first three epochs (episode 1) and then becomes scared with different
#' transition probabilities for the next three epochs (episode 2). The episodes
#' can be solved in reverse order where the value function is used as the
#' terminal values of the preceding episode. This can be done by specifying a
#' vector of horizons (one horizon for each episode) and then lists with
#' transition matrices, observation matrices, and rewards. If the horizon
#' vector has names, then the lists also need to be named, otherwise they have
#' to be in the same order (the numeric index is used). Only the time-varying
#' matrices need to be specified. An example can be found in Example 4 in the
#' Examples section. The procedure can also be done by calling the solver
#' multiple times (see Example 5).
#' 
#' \bold{Note:} The parser for POMDP files is experimental. Please report
#' problems here: \url{https://github.com/mhahsler/pomdp/issues}.
#' 
#' @aliases solve_POMDP solve_POMDP_parameter
#' @param model a POMDP problem specification created with \code{\link{POMDP}}.
#' Alternatively, a POMDP file or the URL for a POMDP file can be specified.
#' @param method string; one of the following solution methods: \code{"grid"},
#' \code{"enum"}, \code{"twopass"}, \code{"witness"}, or \code{"incprune"}.
#' The default is \code{"grid"} implementing the finite grid method.
#' @param horizon an integer with the number of epochs for problems with a
#' finite planning horizon. If set to \code{Inf}, the algorithm continues
#' running iterations till it converges to the infinite horizon solution. If
#' \code{NULL}, then the horizon specified in \code{model} will be used.  For
#' time-dependent POMDPs a vector of horizons can be specified (see Details
#' section).
#' @param discount discount factor in range [0, 1]. If \code{NULL}, then the
#' discount factor specified in \code{model} will be used.
#' @param terminal_values a vector with the terminal values for each state or a
#' matrix specifying the terminal rewards via a terminal value function (e.g.,
#' the alpha component produced by \code{solve_POMDP}).  If \code{NULL}, then
#' the terminal values specified in \code{model} will be used.
#' @param digits precision used when writing POMDP files (see
#' \code{\link{write_POMDP}}).
#' @param parameter a list with parameters passed on to the pomdp-solve
#' program.
#' @param verbose logical, if set to \code{TRUE}, the function provides the
#' output of the pomdp solver in the R console.
#' @return The solver returns an object of class POMDP which is a list with the
#' model specifications (\code{model}), the solution (\code{solution}), and the
#' solver output (\code{solver_output}).
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @references Cassandra, A. (2015). pomdp-solve: POMDP Solver Software,
#' \url{http://www.pomdp.org}.
#' 
#' Sondik, E. (1971). The Optimal Control of Partially Observable Markov
#' Processes. Ph.D. Dissertation, Stanford University.
#' 
#' Cassandra, A., Littman M.L., Zhang L. (1997). Incremental Pruning: A Simple,
#' Fast, Exact Algorithm for Partially Observable Markov Decision Processes.
#' UAI'97: Proceedings of the Thirteenth conference on Uncertainty in
#' artificial intelligence, August 1997, pp. 54-61.
#' 
#' Monahan, G. E. (1982). A survey of partially observable Markov decision
#' processes: Theory, models, and algorithms. Management Science 28(1):1-16.
#' 
#' Littman, M. L.; Cassandra, A. R.; and Kaelbling, L. P. (1996). Efficient
#' dynamic-programming updates in partially observable Markov decision
#' processes. Technical Report CS-95-19, Brown University, Providence, RI.
#' 
#' Zhang, N. L., and Liu, W. (1996). Planning in stochastic domains: Problem
#' characteristics and approximation. Technical Report HKUST-CS96-31,
#' Department of Computer Science, Hong Kong University of Science and
#' Technology.
#' 
#' Pineau J., Geoffrey J Gordon G.J., Thrun S.B. (2003). Point-based value
#' iteration: an anytime algorithm for POMDPs. IJCAI'03: Proceedings of the
#' 18th international joint conference on Artificial Intelligence. Pages
#' 1025-1030.
#' @examples
#' 
#' ################################################################
#' # Example 1: Solving the simple infinite-horizon Tiger problem
#' data("Tiger")
#' Tiger
#' 
#' sol <- solve_POMDP(model = Tiger)
#' sol
#' 
#' # look at the model
#' sol$model
#' 
#' # look at solver output
#' sol$solver_output
#' 
#' # look at the solution
#' sol$solution
#' 
#' # policy (value function (alpha vectors), optimal action and observation dependent transitions)
#' policy(sol)
#' 
#' # plot the policy graph of the infinite-horizon POMDP
#' plot_policy_graph(sol)
#' 
#' # value function
#' plot_value_function(sol, ylim = c(0,20))
#' 
#' # display available solver options which can be passed on to the solver as parameters.
#' solve_POMDP_parameter()
#' 
#' ################################################################
#' # Example 2: Solve a problem specified as a POMDP file
#' #            using a grid of size 10
#' sol <- solve_POMDP("http://www.pomdp.org/examples/cheese.95.POMDP", 
#'   method = "grid", parameter = list(fg_points = 10))
#' sol
#' 
#' policy(sol)
#' 
#' # Example 3: Solving a finite-horizon POMDP using the incremental 
#' #            pruning method (without discounting)
#' sol <- solve_POMDP(model = Tiger, 
#'   horizon = 3, discount = 1, method = "incprune")
#' sol
#' 
#' # look at the policy tree
#' policy(sol)
#' # note: it does not make sense to open the door in epochs 1 or 2 if you only have 3 epochs.
#' 
#' reward(sol) # listen twice and then open the door or listen 3 times
#' reward(sol, belief = c(1,0)) # listen twice (-2) and then open-left (10)
#' reward(sol, belief = c(1,0), epoch = 3) # just open the right door (10)
#' reward(sol, belief = c(.95,.05), epoch = 3) # just open the right door (95% chance)
#' 
#' ################################################################
#' # Example 3: Using terminal values 
#' #
#' # Specify 1000 if the tiger is right after 3 (horizon) epochs
#' sol <- solve_POMDP(model = Tiger, 
#'   horizon = 3, discount = 1,  method = "incprune",
#'   terminal_values = c(0, 1000))
#' sol
#' 
#' policy(sol)
#' # Note: the optimal strategy is never to open the left door, because we think the 
#' # tiger is there then we better wait to get 1000 as the terminal value. If we think 
#' # the Tiger is to the left then open the right door and have a 50/50 chance that the 
#' # Tiger will go to the right door.
#' 
#' ################################################################
#' # Example 4: Model time-dependent transition probabilities 
#' 
#' # The tiger reacts normally for 3 epochs (goes randomly two one
#' # of the two doors when a door was opened). After 3 epochs he gets 
#' # scared and when a door is opened then he always goes to the other door.
#' 
#' # specify the horizon for each of the two differnt episodes
#' Tiger_time_dependent <- Tiger
#' Tiger_time_dependent$model$name <- "Scared Tiger Problem"
#' Tiger_time_dependent$model$horizon <- c(normal_tiger = 3, scared_tiger = 3)
#' Tiger_time_dependent$model$transition_prob <- list(
#'   normal_tiger = list(
#'     "listen" = "identity", 
#'     "open-left" = "uniform", 
#'     "open-right" = "uniform"),
#'   scared_tiger = list(
#'     "listen" = "identity", 
#'     "open-left" = rbind(c(0, 1), c(0, 1)), 
#'     "open-right" = rbind(c(1, 0), c(1, 0))
#'   )
#' )
#' 
#' Tiger_time_dependent
#' 
#' sol <- solve_POMDP(model = Tiger_time_dependent, discount = 1,  method = "incprune")
#' sol
#' 
#' policy(sol)
#' 
#' ################################################################
#' # Example 5: Alternative method to solve time-dependent POMDPs
#' 
#' # 1) create the scared tiger model
#' Tiger_scared <- Tiger
#' Tiger_scared$model$transition_prob <- list(
#'     "listen" = "identity", 
#'     "open-left" = rbind(c(0, 1), c(0, 1)), 
#'     "open-right" = rbind(c(1, 0), c(1, 0))
#'   )
#' 
#' # 2) Solve in reverse order. Scared tiger without terminal values first.
#' sol_scared <- solve_POMDP(model = Tiger_scared, 
#'   horizon = 3, discount = 1,  method = "incprune")
#' sol_scared
#' policy(sol_scared)
#' 
#' # 3) Solve the regular tiger with the value function of the scared tiger as terminal values
#' sol <- solve_POMDP(model = Tiger, 
#'   horizon = 3, discount = 1, method = "incprune",
#'   terminal_values = sol_scared$solution$alpha[[1]])
#' sol
#' policy(sol)
#' # note: it is optimal to mostly listen till the Tiger gets in the scared mood. Only if we are 
#' # extremely sure in the first epoch, then opening a door is optimal.
#' 
#' ################################################################
#' # Example 6: PBVI with a custom grid
#'
#' # Create a search grid by sampling from the belief space in 
#' #   10 regular intervals
#' custom_grid <- sample_belief_space(Tiger, n = 10, method = "regular")
#' custom_grid
#'
#' # Visualize the search grid
#' plot_belief_space(sol, sample = custom_grid)
#'
#' # Solve the POMDP using the grid for approximation 
#' sol <- solve_POMDP(Tiger, method = "grid", parameter = list(grid = custom_grid))
#' sol
#' 
#' @export
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
    model <- read_POMDP(model)
  
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
      warning("The grid method for finite horizon did not converge. The value function and the calculated reward values may not be valid with negative reward in the reward matrix. Use method 'incprune' instead.")
    
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


# is a field time-dependent? For time-dependence we have a list of 
# matrices/data.frames or for observation_prob we have a list of a list
.is_timedependent <- function(x, field) {
  m <- x$model[[field]]
  
  if(is.null(m)) stop("Field ", field, " does not exist.")
  
  if(!is.list(m) || is.data.frame(m)) return(FALSE)
  
  # it is a list. time dependent is a list (episodes) of lists
  if(!is.list(m[[1]])) return(FALSE)
  
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
  
  # check what is time-dependent
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
