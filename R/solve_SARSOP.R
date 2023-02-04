#' Solve a POMDP Problem using SARSOP
#'
#' This function uses the C++ implementation of the SARSOP algorithm
#' by Kurniawati, Hsu and Lee (2008) interfaced in
#' package \pkg{sarsop}
#' to solve infinite horizon problems that are formulated as partially observable Markov
#' decision processes (POMDPs). The result is an optimal or approximately
#' optimal policy.
#'
#' @family policy
#' @family solver
#' @family POMDP
#'
#' @param model a POMDP problem specification created with [POMDP()].
#' Alternatively, a POMDP file or the URL for a POMDP file can be specified.
#' @param method string; there is only one method available called `"sarsop"`.
#' @param horizon need to be `Inf`.
#' @param discount discount factor in range \eqn{[0, 1]}. If `NULL`, then the
#' discount factor specified in `model` will be used.
#' @param terminal_values needs to be `NULL`. SARSOP does not use terminal values.
#' @param digits precision used when writing POMDP files (see
#' [write_POMDP()]).
#' @param parameter a list with parameters passed on to
#' the function [sarsop::pomdpsol()] in package \pkg{sarsop}.
#' @param verbose logical, if set to `TRUE`, the function provides the
#' output of the solver in the R console.
#' @return The solver returns an object of class POMDP which is a list with the
#' model specifications (`'model'`), the solution (`'solution'`), and the
#' solver output (`'solver_output'`).
#' @author Michael Hahsler
#' @references
#' Carl Boettiger, Jeroen Ooms and Milad Memarzadeh (2020). sarsop:
#' Approximate POMDP Planning Software. R package version 0.6.6.
#' https://CRAN.R-project.org/package=sarsop
#'
#' H. Kurniawati, D. Hsu, and W.S. Lee (2008). SARSOP: Efficient point-based POMDP planning by approximating optimally reachable belief spaces. In Proc. Robotics: Science and Systems.
#'
#' @examples
#' \dontrun{
#' # Solving the simple infinite-horizon Tiger problem with SARSOP
#' # You need to install package "sarsop"
#' data("Tiger")
#' Tiger
#'
#' sol <- solve_SARSOP(model = Tiger)
#' sol
#'
#' # look at solver output
#' sol$solver_output
#'
#' # policy (value function (alpha vectors), optimal action and observation dependent transitions)
#' policy(sol)
#'
#' # value function
#' plot_value_function(sol, ylim = c(0,20))
#'
#' # plot the policy graph
#' plot_policy_graph(sol)
#'
#' # reward of the optimal policy
#' reward(sol)
#'
#' # Solve a problem specified as a POMDP file. The timeout is set to 10 seconds.
#' sol <- solve_SARSOP("http://www.pomdp.org/examples/cheese.95.POMDP", parameter = list(timeout = 10))
#' sol
#' }
#'
#' @export
solve_SARSOP <- function(model,
  horizon = Inf,
  discount = NULL,
  terminal_values = NULL,
  method = "sarsop",
  digits = 7,
  parameter = NULL,
  verbose = FALSE) {
  check_installed("sarsop")
  
  #if (!requireNamespace("sarsop", quietly = TRUE)) {
  #  stop("Package \"sarsop\" needed for this function to work. Please install it.",
  #    call. = FALSE)
  #}
  
  if (method != "sarsop")
    stop("Only available method: 'sarsop'")
  
  # check parameters
  if (!is.null(terminal_values))
    stop("the SARSOP solver does not support terminal values.")
  
  # do we have a model POMDP file?
  if (is.character(model))
    model <- read_POMDP(model)
  
  if (!is.null(model$horizon) &&
      !is.infinite(model$horizon))
    warning(
      "Replacing the horizon specified in the model (",
      model$horizon,
      ") with infinity. SARSOP only solves infinite-horizon problems."
    )
  
  model$horizon <- horizon
  
  if (!is.infinite(model$horizon))
    stop("SARSOP only solves infinite-horizon problems.")
  
  if (!is.null(horizon))
    model$horizon <- horizon
  
  if (!is.infinite(model$horizon))
    stop("the SARSOP solver only supports infinite time horizon problems.")
  
  if (!is.null(discount))
    model$discount <- discount
  
  if (is_timedependent_POMDP(model))
    stop("the SARSOP solver does not support time dependent models.")
  
  # prepare temp files
  tmpf <- tempfile()
  model_file <- paste0(tmpf, '.pomdp')
  policy_file <- paste0(tmpf, '.policyx')
  log_file <- paste0(tmpf, '.log')
  
  # write model POMDP file
  if (!is.null(model$problem))
    writeLines(model$problem, con = model_file)
  else
    write_POMDP(model, model_file, digits = digits)
  
  
  # call SARSOP
  res <- do.call(sarsop::pomdpsol, c(
    list(
      model = model_file,
      output = policy_file,
      stdout = log_file,
      stderr = log_file,
      spinner = FALSE
    ),
    parameter
  ))
  
  model$solver_output = readLines(log_file)
  class(model$solver_output) <- "text"
  if (verbose)
    print(model$solver_output, "\n")
  
  # package solution
  policy <- sarsop::read_policyx(policy_file)
  pg <- data.frame(
    node = seq_along(policy$action),
    action = factor(model$actions[policy$action], levels = model$actions)
  )
  alpha <- t(policy$vectors)
  colnames(alpha) = model$states
  
  policy <- data.frame(alpha, action = pg$action)
  
  model$solution <- structure(
    list(
      method = "sarsop",
      parameter = parameter,
      horizon = model$horizon,
      discount = model$discount,
      converged = length(grep("precision reached", res$end_condition)) == 1,
      total_expected_reward = NA,
      initial_belief = model$start,
      initial_pg_node = NA,
      #  terminal_values = if(!is.null(terminal_values)) terminal_values else 0,
      #belief_states = belief,
      pg = list(pg),
      alpha = list(alpha),
      policy = list(policy)
    ),
    class = "POMDP_solution"
  )
  
  model$solution$total_expected_reward = reward(model, model$start)
  model$solution$initial_pg_node <- reward_node_action(model)$pg_node
  
  model
}
