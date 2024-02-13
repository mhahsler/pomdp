#' Define an MDP Problem
#'
#' Defines all the elements of a finite state-space MDP problem.
#'
#' Markov decision processes (MDPs) are discrete-time stochastic control
#' process with completely observable states. We implement here
#' MDPs with a finite state space. similar to [POMDP]
#' models, but without the observation model. The `'observations'` column in
#' the the reward specification is always missing.
#'
#' `MDP2POMDP()` reformulates an MDP as a POMDP by adding an observation
#' model with one observation per state
#' that reveals the current state. This is achieved by adding identity
#' observation probability matrices.
#'
#' More details on specifying the model components can be found in the documentation
#' for [POMDP].
#'
#' @family MDP
#'
#' @include POMDP.R
#' @param states a character vector specifying the names of the states.
#' @param actions a character vector specifying the names of the available
#' actions.
#' @param transition_prob Specifies the transition probabilities between
#' states.
#' @param reward Specifies the rewards dependent on action, states and
#' observations.
#' @param discount numeric; discount rate between 0 and 1.
#' @param horizon numeric; Number of epochs. `Inf` specifies an infinite
#' horizon.
#' @param start Specifies in which state the MDP starts.
#' @param normalize logical; should the description be normalized for faster access (see [normalize_MDP()])?
#' @param info A list with additional information.
#' @param name a string to identify the MDP problem.
#' @param x a `MDP` object.
#'
#' @return The function returns an object of class MDP which is list with
#'   the model specification. [solve_MDP()] reads the object and adds a list element called
#' `'solution'`.
#' @author Michael Hahsler
#' @examples
#' # Michael's Sleepy Tiger Problem is like the POMDP Tiger problem, but
#' # has completely observable states because the tiger is sleeping in front
#' # of the door. This makes the problem an MDP.
#'
#' STiger <- MDP(
#'   name = "Michael's Sleepy Tiger Problem",
#'   discount = .9,
#'
#'   states = c("tiger-left" , "tiger-right"),
#'   actions = c("open-left", "open-right", "do-nothing"),
#'   start = "uniform",
#'
#'   # opening a door resets the problem
#'   transition_prob = list(
#'     "open-left" =  "uniform",
#'     "open-right" = "uniform",
#'     "do-nothing" = "identity"),
#'
#'   # the reward helper R_() expects: action, start.state, end.state, observation, value
#'   reward = rbind(
#'     R_("open-left",  "tiger-left",  v = -100),
#'     R_("open-left",  "tiger-right", v =   10),
#'     R_("open-right", "tiger-left",  v =   10),
#'     R_("open-right", "tiger-right", v = -100),
#'     R_("do-nothing",                v =    0)
#'   )
#' )
#'
#' STiger
#'
#' sol <- solve_MDP(STiger)
#' sol
#'
#' policy(sol)
#' plot_value_function(sol)
#'
#' # convert the MDP into a POMDP and solve
#' STiger_POMDP <- MDP2POMDP(STiger)
#' sol2 <- solve_POMDP(STiger_POMDP)
#' sol2
#'
#' policy(sol2)
#' plot_value_function(sol2, ylim = c(80, 120))
#' @export
MDP <- function(states,
                actions,
                transition_prob,
                reward,
                discount = .9,
                horizon = Inf,
                start = "uniform",
                normalize = TRUE,
                info = NULL,
                name = NA) {
  
  # MDP does not have observations
  if (is.data.frame(reward))
    reward$observation <- NULL
  
  x <- list(
    name = name,
    discount = discount,
    horizon = horizon,
    states = states,
    actions = actions,
    transition_prob = transition_prob,
    reward = reward,
    info = info,
    start = start
  )
  
  class(x) <- list("MDP", "list")
  x <- check_and_fix_MDP(x)
  
  if (normalize)
    x <- normalize_MDP(x)
  
  x
}


#' @export
print.MDP <- function(x, ...) {
  writeLines(paste(paste(class(x), collapse = ", "),
                   "-",
                   x$name))
  
  if (!is.null(x$discount))
    writeLines(sprintf("  Discount factor: %s",
                       paste(x$discount, collapse = "+")))
  
  if (!is.null(x$horizon))
    writeLines(sprintf("  Horizon: %s epochs",
                       paste(x$horizon, collapse = " + ")))
  
  writeLines(sprintf(
    "  Size: %d states / %d actions",
    length(x$states),
    length(x$actions)
  ))
  
  writeLines(paste0("  Start: ", shorten(paste(
    x$start, collapse = ", "
  ), n = -10L)))
  
  if (is_solved_MDP(x))
    writeLines(c(
      "  Solved:",
      sprintf("    Method: %s",
              sQuote(x$solution$method)),
      sprintf("    Solution converged: %s",
              x$solution$converged)
    ))
  
  writeLines("")
  
  writeLines(strwrap(
    paste("List components:", paste(sQuote(names(
      x
    )), collapse = ", "), "\n"),
    indent = 2,
    exdent = 4
  ))
}

#' @rdname MDP
#' @param stop logical; stop with an error.
#' @export
is_solved_MDP <- function(x, stop = FALSE) {
  if (!inherits(x, "MDP"))
    stop("x needs to be a MDP object!")
  solved <- !is.null(x$solution)
  if (stop && !solved)
    stop("x needs to be a solved MDP. Use solve_MDP() first.")
  
  solved
}

## this is .get_pg_index for MDPs
.get_pol_index <- function(model, epoch) {
  epoch <- as.integer(epoch)
  if (epoch < 1L)
    stop("Epoch has to be >= 1")
  
  ### (converged) infinite horizon POMDPs. We ignore epoch.
  if (length(model$solution$policy) == 1L)
    return(1L)
  
  ### regular epoch for finite/infinite horizon case
  if (epoch > length(model$solution$policy))
    stop("MDP model has only a policy up to epoch ",
         length(model$solution$policy))
  
  return(epoch)
}

#' @rdname MDP
#' @export
MDP2POMDP <- function(x) {
  if (!inherits(x, "MDP"))
    stop("'x' needs to be of class 'MDP'.")
  
  # add an observation for each state and identity observation_probability for all actions ('*')
  # (note: pomdp-solve does not support "identity" for observation_probs)
  x$observations <- x$states
  ident_matrix <- diag(length(x$states))
  dimnames(ident_matrix) <- list(x$states, x$observations)
  
  x$observation_prob <-
    sapply(
      x$actions,
      FUN = function(x)
        ident_matrix,
      simplify = FALSE
    )
 
  # add missing observations to reward data.frame
  if(is.data.frame(x$reward))
    x$reward <- data.frame(action = x$reward$action, 
                           start.state = x$reward$start.state, 
                           end.state = x$reward$end.state, 
                           observation = factor(NA_character_, levels = x$states), 
                           value = x$reward$value)
   
  class(x) <- c("POMDP", "list")
  check_and_fix_MDP(x)
}

