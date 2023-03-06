#' Solve a POMDP Problem using pomdp-solver
#'
#' This function utilizes the C implementation of 'pomdp-solve' by Cassandra
#' (2015) to solve problems that are formulated as partially observable Markov
#' decision processes (POMDPs). The result is an optimal or approximately
#' optimal policy.
#'
#' ## Parameters
#' `solve_POMDP_parameter()` displays available solver parameter options.
#'
#' **Horizon:** Infinite-horizon POMDPs (`horizon = Inf`) converge to a
#' single policy graph. Finite-horizon POMDPs result in a policy tree of a
#' depth equal to the smaller of the horizon or the number of epochs to
#' convergence.  The policy (and the associated value function) are stored in a
#' list by epoch. The policy for the first epoch is stored as the first
#' element. Horizon can also be used to limit the number of epochs used 
#' for value iteration.
#'
#' *Precision:** The POMDP solver uses various epsilon values to control
#' precision for comparing alpha vectors to check for convergence, and solving
#' LPs. Overall precision can be changed using
#' `parameter = list(epsilon = 1e-3)`.
#'
#' **Methods:** Several algorithms using exact value iteration are
#' available:
#'
#' * Enumeration (Sondik 1971).
#' * Two pass (Sondik 1971).
#' * Witness (Littman, Cassandra, Kaelbling, 1996).
#' * Incremental pruning (Zhang and Liu, 1996, Cassandra et al 1997).
#' 
#' In addition, the following approximate value iteration method is available:
#' * Grid implements a variation of point-based value iteration
#'   to solve larger POMDPs (PBVI; see Pineau 2003) without dynamic belief set expansion.
#'
#' Details can be found in (Cassandra, 2015).
#'
#' **Note on method grid:** The grid method implements a version of Point
#' Based Value Iteration (PBVI). The used belief points are by default created
#' using points that are reachable from the initial belief (`start`) by
#' following all combinations of actions and observations. The size of the grid
#' can be set via `parameter = list(fg_points = 100)`. Alternatively,
#' different strategies can be chosen using the parameter `fg_type`. In
#' this implementation, the user can also specify manually a grid of belief
#' states by providing a matrix with belief states as produced by
#' [sample_belief_space()] as the parameter `grid`.
#'
#' To guarantee convergence in point-based (finite grid) value iteration, the
#' initial value function must be a lower bound on the optimal value function.
#' If all rewards are strictly non-negative, an initial value function with an
#' all zero vector can be used and results will be similar to other methods.
#' However, if there are negative rewards, lower bounds can be guaranteed by
#' setting a single vector with the values \eqn{min(reward)/(1 - discount)}.
#' The value function is guaranteed to converge to the true value function, but
#' finite-horizon value functions will not be as expected. [solve_POMDP()]
#' produces a warning in this case.
#'
#' **Time-dependent POMDPs:** Time dependence of transition probabilities,
#' observation probabilities and reward structure can be modeled by considering
#' a set of episodes representing epochs with the same settings. In the scared
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
#' ## Solution
#'
#' **Policy:**
#' Each policy is a data frame where each row representing a
#' policy graph node with an associated optimal action and a list of node IDs
#' to go to depending on the observation (specified as the column names). For
#' the finite-horizon case, the observation specific node IDs refer to nodes in
#' the next epoch creating a policy tree.  Impossible observations have a
#' `NA` as the next state.
#'
#' **Value function:**
#' The value function specifies the value of the value function (the expected reward)
#' over the belief space. The dimensionality of the belief space is $n-1$ where $n$ is the number of states.
#' The value function is stored as a matrix. Each row is
#' associated with a node (row) in the policy graph and represents the
#' coefficients (alpha or V vector) of a hyperplane. It contains one
#' value per state which is the value for the belief state that has a probability
#' of 1 for that state and 0s for all others.
#'
#' @family policy
#' @family solver
#' @family POMDP
#'
#' @param model a POMDP problem specification created with [POMDP()].
#' Alternatively, a POMDP file or the URL for a POMDP file can be specified.
#' @param method string; one of the following solution methods: `"grid"`,
#' `"enum"`, `"twopass"`, `"witness"`, or `"incprune"`.
#' The default is `"grid"` implementing the finite grid method.
#' @param horizon an integer with the number of epochs for problems with a
#' finite planning horizon. If set to `Inf`, the algorithm continues
#' running iterations till it converges to the infinite horizon solution. If
#' `NULL`, then the horizon specified in `model` will be used.  For
#' time-dependent POMDPs a vector of horizons can be specified (see Details
#' section).
#' @param discount discount factor in range \eqn{[0, 1]}. If `NULL`, then the
#' discount factor specified in `model` will be used.
#' @param initial_belief An initial belief vector. If `NULL`, then the
#' initial belief specified in `model` (as start) will be used.
#' @param terminal_values a vector with the terminal utility values for each state or a
#' matrix specifying the terminal rewards via a terminal value function (e.g.,
#' the alpha components produced by [solve_POMDP()]).  If `NULL`, then, if available,
#' the terminal values specified in `model` will be used or a vector with all 0s otherwise.
#' @param digits precision used when writing POMDP files (see
#' [write_POMDP()]).
#' @param parameter a list with parameters passed on to the pomdp-solve
#' program.
#' @param timeout number of seconds for the solver to run.
#' @param verbose logical, if set to `TRUE`, the function provides the
#' output of the pomdp solver in the R console.
#' @return The solver returns an object of class POMDP which is a list with the
#' model specifications. Solved POMDPs also have an element called `solution` which is a list, and the
#' solver output (`solver_output`). The solution is a list that contains elements like:
#' - `method` used solver method.
#' - `solver_output` output of the solver program.
#' - `converged` did the solution converge?
#' - `initial_belief` used initial belief used.
#' - `total_expected_reward` total expected reward starting from the the initial belief.
#' - `pg`, `initial_pg_node` the policy graph (see Details section).
#' - `alpha` value function as hyperplanes representing the nodes in the policy graph (see Details section).
#' - `belief_points_solver` optional; belief points used by the solver.
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @references
#' Cassandra, A. (2015). pomdp-solve: POMDP Solver Software,
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
#' # display available solver options which can be passed on to pomdp-solve as parameters.
#' solve_POMDP_parameter()
#'
#' ################################################################
#' # Example 1: Solving the simple infinite-horizon Tiger problem
#' data("Tiger")
#' Tiger
#'
#' # look at the model as a list
#' unclass(Tiger)
#'
#' # inspect an individual field of the model (e.g., the transition probabilities and the reward)
#' Tiger$transition_prob
#' Tiger$reward
#'
#' sol <- solve_POMDP(model = Tiger)
#' sol
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
#' ################################################################
#' # Example 2: Solve a problem specified as a POMDP file
#' #            using a grid of size 20
#' sol <- solve_POMDP("http://www.pomdp.org/examples/cheese.95.POMDP",
#'   method = "grid", parameter = list(fg_points = 20))
#' sol
#'
#' policy(sol)
#' plot_policy_graph(sol)
#'
#' # Example 3: Solving a finite-horizon POMDP using the incremental
#' #            pruning method (without discounting)
#' sol <- solve_POMDP(model = Tiger,
#'   horizon = 3, discount = 1, method = "incprune")
#' sol
#'
#' # look at the policy tree
#' policy(sol)
#' plot_policy_graph(sol)
#' # note: only open the door in epoch 3 if you get twice the same observation.
#'
#' # Expected reward starting for the models initial belief (uniform):
#' #   listen twice and then open the door or listen 3 times
#' reward(sol)
#'
#' # Expected reward for listen twice (-2) and then open-left (-1 + (-1) + 10 = 8)
#' reward(sol, belief = c(1,0))
#'
#' # Expected reward for just opening the right door (10)
#' reward(sol, belief = c(1,0), epoch = 3)
#'
#' # Expected reward for just opening the right door (0.5 * -100 + 0.95 * 10 = 4.5)
#' reward(sol, belief = c(.95,.05), epoch = 3)
#'
#' ################################################################
#' # Example 3: Using terminal values (state-dependent utilities after the final epoch)
#' #
#' # Specify 1000 if the tiger is right after 3 (horizon) epochs
#' sol <- solve_POMDP(model = Tiger,
#'   horizon = 3, discount = 1,  method = "incprune",
#'   terminal_values = c(0, 1000))
#' sol
#'
#' policy(sol)
#' # Note: The optimal strategy is to never open the left door. If we think the
#' #  Tiger is behind the right door, then we just wait for the final payout. If
#' #  we think the tiger might be behind the left door, then we open the right
#' #  door, are likely to get a small reward and the tiger has a chance of 50\% to
#' #  move behind the right door. The second episode is used to gather more
#' #  information for the more important #  final action.
#'
#' ################################################################
#' # Example 4: Model time-dependent transition probabilities
#'
#' # The tiger reacts normally for 3 epochs (goes randomly two one
#' # of the two doors when a door was opened). After 3 epochs he gets
#' # scared and when a door is opened then he always goes to the other door.
#'
#' # specify the horizon for each of the two different episodes
#' Tiger_time_dependent <- Tiger
#' Tiger_time_dependent$name <- "Scared Tiger Problem"
#' Tiger_time_dependent$horizon <- c(normal_tiger = 3, scared_tiger = 3)
#' Tiger_time_dependent$transition_prob <- list(
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
#' # Tiger_time_dependent (a higher value for verbose will show more messages)
#'
#' sol <- solve_POMDP(model = Tiger_time_dependent, discount = 1,
#'   method = "incprune", verbose = 1)
#' sol
#'
#' policy(sol)
#'
#' # note that the default method to estimate the belief for nodes is following a
#' #  trajectory which uses only the first belief reached for each node. Random sampling
#' #  can find a better estimate of the central belief of the segment (see nodes 4-1 to 6-3
#' #  in the plots below).
#' plot_policy_graph(sol)
#' plot_policy_graph(sol, method = "random_sample")
#'
#' ################################################################
#' # Example 5: Alternative method to solve time-dependent POMDPs
#'
#' # 1) create the scared tiger model
#' Tiger_scared <- Tiger
#' Tiger_scared$transition_prob <- list(
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
#' # Note: it is optimal to mostly listen till the Tiger gets in the scared mood. Only if
#' #  we are extremely sure in the first epoch, then opening a door is optimal.
#'
#' ################################################################
#' # Example 6: PBVI with a custom grid
#'
#' # Create a search grid by sampling from the belief space in
#' #   100 regular intervals
#' custom_grid <- sample_belief_space(Tiger, n = 100, method = "regular")
#' head(custom_grid)
#'
#' # Visualize the search grid
#' plot_belief_space(sol, sample = custom_grid)
#'
#' # Solve the POMDP using the grid for approximation
#' sol <- solve_POMDP(Tiger, method = "grid", parameter = list(grid = custom_grid))
#' policy(sol)
#' plot_policy_graph(sol)
#'
#' # note that plot_policy_graph() automatically remove nodes that are unreachable from the
#' #  initial node. This behavior can be switched off.
#' plot_policy_graph(sol, remove_unreachable_nodes = FALSE)
#' @import pomdpSolve
#' @importFrom processx run
#' @export
solve_POMDP <- function(model,
  horizon = NULL,
  discount = NULL,
  initial_belief = NULL,
  terminal_values = NULL,
  method = "grid",
  digits = 7,
  parameter = NULL,
  timeout = Inf,
  verbose = FALSE) {
  converged <- NA
  
  methods <- c("grid", "enum", "twopass", "witness", "incprune")
  # Not available (linsup need CPLEX and mcgs is not finished):  "linsup", "mcgs")
  method <- match.arg(method, methods)
  
  # do we have a model POMDP file?
  if (is.character(model))
    model <- read_POMDP(model)
  
  if (is.null(horizon))
    horizon <- model$horizon
  if (is.null(horizon))
    horizon <- Inf
  model$horizon <- horizon
  
  if (!is.null(initial_belief)) {
    model$start <- initial_belief
  }
  
  if (is.null(model$start))
    model$start <- "uniform"
  
  # time-dependent POMDP? (horizon is a vector of length >1)
  if (is_timedependent_POMDP(model))
    return(
      .solve_POMDP_time_dependent(
        model = model,
        horizon = horizon,
        discount = discount,
        terminal_values = terminal_values,
        method = method,
        parameter = parameter,
        verbose = verbose
      )
    )
  
  # horizon should now be length 1
  if (horizon < 1)
    horizon <- Inf
  else
    if (horizon != floor(horizon))
      stop("'horizon' needs to be an integer.")
  
  if (is.null(terminal_values))
    terminal_values <- model$terminal_values
  if (!is.null(terminal_values) &&
      length(terminal_values) == 1 &&
      terminal_values == 0)
    terminal_values <- NULL
  model$terminal_values <- terminal_values
  
  if (is.null(discount))
    discount <- model$discount
  if (is.null(discount)) {
    message("No discount rate specified. Using .9!")
    discount <- .9
  }
  model$discount <- discount
  
  ### temp file names
  file_prefix <- tempfile(pattern = "pomdp_")
  pomdp_filename <- paste0(file_prefix, ".POMDP")
  
  # write model POMDP file
  if (verbose)
    cat("Writing the problem to", pomdp_filename, "\n")
  if (is.null(model$transition_prob) ||
      is.null(model$observation_prob) || is.null(model$reward))
    writeLines(model$problem, con = pomdp_filename)
  else
    write_POMDP(model, pomdp_filename, digits = digits)
  
  # write terminal values file
  if (!is.null(terminal_values)) {
    if (!is.matrix(terminal_values))
      terminal_values <- rbind(terminal_values)
    if (ncol(terminal_values) != length(model$states))
      stop("number of terminal values does not match the number of states.")
    colnames(terminal_values) <- as.character(model$states)
    
    terminal_values_filename <-
      .write_alpha_file(file_prefix, terminal_values)
  }
  
  # write grid file for method grid
  if (!is.null(parameter$grid)) {
    if (method != "grid")
      warning("Custom grids are ignored by all methods but 'grid'!")
    
    # TODO: check grid
    parameter$grid_filename <-
      .write_grid_file(file_prefix, parameter$grid)
    parameter$grid <- NULL
    parameter$fg_type <- "file"
  }
  
  # construct parameter string
  if (!is.null(parameter)) {
    paras <- as.vector(sapply(
      names(parameter),
      FUN = function(n)
        c(paste0("-", n), if (is.logical(parameter[[n]])) {
          if (parameter[[n]])
            "true"
          else
            "false"
        } else
          parameter[[n]])
    ))
  } else
    paras <- NULL
  
  # for verbose it goes directly to the console ("")
  # for finite horizon we need to save all pg and alpha files
  pomdp_args <- c("-pomdp", pomdp_filename,
    "-method", method)
  
  if (method == "grid")
    pomdp_args <- append(pomdp_args,
      c("-fg_save", "true"))
  
  if (is.finite(horizon))
    pomdp_args <- append(pomdp_args,
      c("-horizon", horizon,
        "-save_all", "true"))
  
  if (!is.null(discount))
    pomdp_args <- append(pomdp_args, c("-discount", discount))
  
  if (!is.null(terminal_values))
    pomdp_args <-
    append(pomdp_args,
      c("-terminal_values", terminal_values_filename))
  
  if (!is.null(paras))
    pomdp_args <- append(pomdp_args, paras)
  
  if (verbose)
    cat(
      "Calling pomdp-solve with the following arguments:",
      paste(pomdp_args, collapse =  " "),
      "\nSolver output:",
      sep = "\n"
    )
  
  # solver_output <- system2(
  #   pomdpSolve::find_pomdp_solve(),
  #   args = pomdp_args,
  #   stdout = ifelse(verbose, "", TRUE),
  #   stderr = ifelse(verbose, "", TRUE),
  #   wait = TRUE
  # )
  
  # if (!is.null(attr(solver_output, "status")) ||
  #     (verbose && solver_output != 0)) {
  #   if (!verbose)
  #     cat(paste(solver_output, "\n\n"))
  #
  #   # message(
  #   #   "Note: The action and state index reported by the solver above starts with 0 and not with 1:\n"
  #   # )
  #   #
  #   # m <- max(
  #   #   length(model$states),
  #   #   length(model$actions),
  #   #   length(model$observations)
  #   # )
  #   #
  #   # print(
  #   #   data.frame(
  #   #     index = (1:m) - 1,
  #   #     action = model$actions[1:m],
  #   #     state = model$states[1:m],
  #   #     observation = model$observations[1:m]
  #   #   )
  #   # )
  #   # cat("\n")
  #
  #   message("Debugging info: The used POMDP definition file can be found at: ", pomdp_filename,
  #     "\n  use file.show('", pomdp_filename, "') to see the POMDP file.")
  #
  #   stop("POMDP solver returned an error (see above).")
  # }
  
  solver_output <- processx::run(
    pomdpSolve::find_pomdp_solve(),
    args = pomdp_args,
    echo = verbose,
    timeout = timeout,
    error_on_status = FALSE
  )
  
  if (solver_output$status != 0 && !verbose)
    cat(paste(solver_output$stdout))
  
  # solver did not finish (currently only works for infinite horizon)
  if (solver_output$timeout) {
    cat(" timeout reached!\n")
    
    if (is.finite(horizon))
      stop("Unfinished solutions cannot be used for finite horizon problems!")
    
    ep <- strsplit(solver_output$stdout, "\n")[[1]]
    ep <- ep[length(ep) - 1]
    ep <- as.integer(gsub("Epoch: (\\d+).*", "\\1", ep))
    if (is.na(ep))
      stop("Could not find a solved epoch. You may need to increase the timeout.")
    
    cat("Trying to load last solved epoch: Epoch ", ep, "\n\n")
    
    converged <- FALSE
    alpha <- list(.get_alpha_file(file_prefix, model, ep))
    pg <- list(.get_pg_file(file_prefix, model, ep))
  }
  
  
  ## converged infinite horizon POMDPs produce a policy graph
  else if (!is.finite(horizon)) {
    converged <- TRUE
    alpha <- list(.get_alpha_file(file_prefix, model))
    pg <- list(.get_pg_file(file_prefix, model))
    
  } else{
    ## finite horizon pomdp: read the policy tree
    converged <- FALSE ### did the grid method converge?
    alpha <- list()
    pg <- list()
    for (i in 1:horizon) {
      ## no more files exist after convergence
      r <- suppressWarnings(try({
        alpha[[i]] <- .get_alpha_file(file_prefix, model, i)
        pg[[i]] <- .get_pg_file(file_prefix, model, i)
      }, silent = TRUE)
      )
      if (inherits(r, "try-error"))
      {
        if (verbose)
          cat("Convergence: Finite-horizon POMDP converged early at epoch:",
            i  -  1,
            "\n")
        converged <- TRUE
        
        # we only need to keep the first pg element with the graph
        pg <- tail(pg, n = 1L)
        alpha <- tail(alpha, n = 1L)
        
        break
      }
    }
    
    ## make transitions in last epoch NA for non converged solutions
    # we need this for timedependent POMDPs
    #if (!converged)
    #pg[[1L]][, as.character(model$observations)] <- NA
    
    ## order by epoch
    alpha <- rev(alpha)
    pg <- rev(pg)
    
    if (method == "grid" &&
        !converged &&
        any(unlist(reward_matrix(model)) < 0))
      warning(
        "The grid method for finite horizon did not converge. The value function and the calculated reward values may not be valid with negative reward in the reward matrix. Use method 'simulate_POMDP()' to estimate the reward or use solution method 'incprune'."
      )
    
  }
  
  # read belief states if available (method: grid)
  belief <- .get_belief_file(file_prefix, model)
  
  # add solution to model
  model$solution <- structure(
    list(
      method = method,
      parameter = parameter,
      converged = converged,
      total_expected_reward = NA,
      initial_belief = NA,
      initial_pg_node = NA,
      belief_points_solver = belief,
      pg = pg,
      alpha = alpha
    ),
    class = "POMDP_solution"
  )
  
  ## add initial node and reward
  rew <- reward_node_action(model, belief = model$start)
  model$solution$initial_belief <- rew$belief
  model$solution$total_expected_reward <- rew$reward
  model$solution$initial_pg_node <- rew$pg_node
  
  model$solution$solver_output <-
    structure(solver_output, class = "text")
  
  ### MDP uses policy field
  if (inherits(model, "MDP"))
    model$solution$policy <- .MDP_policy_from_POMDP(model)
  
  model
}


print.POMDP_solution <- function(x, ...) {
  cat("POMDP solution\n\n")
  print(unclass(x))
}


# solve time-dependent POMDP
# we can have different transition_probs, observation_probs or rewards
.solve_POMDP_time_dependent <-
  function(model,
    horizon = NULL,
    ...,
    terminal_values = NULL,
    verbose = FALSE) {
    if (verbose)
      cat("\n+++++++++ time-dependent POMDP +++++++++\n", sep = "")
    
    if (is.null(horizon))
      horizon <- model$horizon
    n <- length(horizon)
    
    # check what is time-dependent
    do_trans <- .is_timedependent_field(model, "transition_prob")
    do_obs <- .is_timedependent_field(model, "observation_prob")
    do_reward <- .is_timedependent_field(model, "reward")
    
    if (verbose) {
      if (do_trans)
        cat(" * Using time-dependent transition probabilities.\n")
      if (do_obs)
        cat(" * Using time-dependent observation probabilities.\n")
      if (do_reward)
        cat(" * Using time-dependent rewards.\n")
    }
    
    # solve POMDPS in reverse order
    s <- list()
    m <- model
    if (!is.null(terminal_values))
      m$terminal_values <- terminal_values
    
    for (i in seq(n, 1)) {
      if (i < n)
        m$terminal_values <- prev_alpha
      m$horizon <- model$horizon[i]
      
      # match names instead of index?
      if (is.null(names(m$horizon)))
        take <- i
      else
        take <- names(m$horizon)
      
      if (verbose)
        cat(
          "\n++++++++++++++++++++++++++++++++++++++++\n",
          "Solving episode ",
          i,
          " of ",
          n,
          " (",
          take,
          ") with horizon ",
          m$horizon,
          "\n",
          sep = ""
        )
      
      
      if (do_trans)
        m$transition_prob <-
        model$transition_prob[[take]]
      if (do_obs)
        m$observation_prob <-
        model$observation_prob[[take]]
      if (do_reward)
        m$reward <- model$reward[[take]]
      
      if (verbose > 1) {
        if (do_trans) {
          cat("Using transition probabilities:\n")
          print(m$transition_prob)
        }
      }
      
      s[[i]] <- solve_POMDP(m, ..., verbose = verbose > 2)
      prev_alpha <- s[[i]]$solution$alpha[[1]]
    }
    
    # combine the results
    model$solution <- s[[1]]$solution
    
    model$solution$pg <-
      unlist(lapply(
        s,
        FUN = function(x)
          x$solution$pg
      ), recursive = FALSE)
    
    model$solution$alpha <-
      unlist(lapply(
        s,
        FUN = function(x)
          x$solution$alpha
      ), recursive = FALSE)
    
    model$solution$policy <-
      unlist(lapply(
        s,
        FUN = function(x)
          x$solution$policy
      ), recursive = FALSE)
    
    model$solution$total_expected_reward <-
      sum(sapply(
        s,
        FUN = function(x)
          x$solution$total_expected_reward
      ))
    
    # we do not support belief_points
    model$solution$belief_points_solver <- NULL
    
    model
  }

#' @rdname solve_POMDP
#' @export
solve_POMDP_parameter <- function() {
  solver_output <- system2(
    pomdpSolve::find_pomdp_solve(),
    args = c("-h"),
    stdout = TRUE,
    stderr = TRUE,
    wait = TRUE
  )
  
  cat(solver_output, sep = "\n")
  cat(
    "\nUse the parameter options in solve_POMDP without the leading '-' in the form:",
    "\tparameter = list(fg_points = 100)",
    "Note: Not all parameter options are available (e.g., resource limitations, -pomdp, -horizon).",
    sep = "\n"
  )
}
