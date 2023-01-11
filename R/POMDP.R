#' Define a POMDP Problem
#'
#' Defines all the elements of a POMDP problem including the discount rate, the
#' set of states, the set of actions, the set of observations, the transition
#' probabilities, the observation probabilities, and rewards.
#'
#' In the following we use the following notation. The POMDP is a 7-duple:
#'
#' \eqn{(S,A,T,R, \Omega ,O, \gamma)}.
#'
#' \eqn{S} is the set of states; \eqn{A}
#' is the set of actions; \eqn{T} are the conditional transition probabilities
#' between states; \eqn{R} is the reward function; \eqn{\Omega} is the set of
#' observations; \eqn{O} are the conditional observation probabilities; and
#' \eqn{\gamma} is the discount factor. We will use lower case letters to
#' represent a member of a set, e.g., \eqn{s} is a specific state. To refer to
#' the size of a set we will use cardinality, e.g., the number of actions is
#' \eqn{|A|}.
#'
#' **Names used for mathematical symbols in code**
#'
#' * \eqn{S, s, s'}: `'states', start.state', 'end.state'`
#' * \eqn{A, a}: `'actions', 'action'`
#' * \eqn{\Omega, o}: `'observations', 'observation'`
#'
#' State names, actions and observations can be specified as strings or index numbers
#' (e.g., `start.state` can be specified as the index of the state in `states`).
#' For the specification as data.frames below, `'*'` or `NA` can be used to mean
#' any  `start.state`, `end.state`, `action` or `observation`. Note that `'*'` is internally
#' always represented as an `NA`.
#'
#' The specification below map to the format used by pomdp-solve
#' (see \url{http://www.pomdp.org}).
#'
#' **Specification of transition probabilities: \eqn{T(s' | s, a)}**
#'
#' Transition probability to transition to state \eqn{s'} from given state \eqn{s}
#' and action \eqn{a}. The transition probabilities can be
#' specified in the following ways:
#'
#' * A data.frame with columns exactly like the arguments of `T_()`.
#'   You can use `rbind()` with helper function `T_()` to create this data
#'   frame.
#'
#' * A named list of matrices, one for each action. Each matrix is square with
#'   rows representing start states \eqn{s} and columns representing end states \eqn{s'}.
#'   Instead of a matrix, also the strings `'identity'` or `'uniform'` can be specified.
#'
#' * A function with the same arguments are `T_()`, but no default values
#'   that returns the transition probability.
#'
#' **Specification of observation probabilities: \eqn{O(o | s', a)}**
#'
#' The POMDP specifies the probability for each observation \eqn{o} given an
#' action \eqn{a} and that the system transitioned to the end state
#' \eqn{s'}. These probabilities can be specified in the
#' following ways:
#'
#' * A data frame with columns named exactly like the arguments of `O_()`.
#'   You can use `rbind()`
#'   with helper function `O_()` to create this data frame.
#'
#' * A named list of matrices, one for each action. Each matrix has
#'   rows representing end states \eqn{s'} and columns representing an observation \eqn{o}.
#'   Instead of a matrix, also the strings `'identity'` or `'uniform'` can be specified.
#'
#' * A function with the same arguments are `O_()`, but no default values
#'   that returns the observation probability.
#'
#' **Specification of the reward function: \eqn{R(s, s', o, a)}**
#'
#' The reward function can be specified in the following
#' ways:
#'
#' * A data frame with columns named exactly like the arguments of `R_()`.
#'   You can use `rbind()`
#'   with helper function `R_()` to create this data frame.
#'
#' * A list of lists. The list levels are `'action'` and `'start.state'`. The list elements
#'   are matrices with
#'   rows representing end states \eqn{s'} and columns representing an observation \eqn{o}.
#'
#' * A function with the same arguments are `R_()`, but no default values
#'   that returns the reward.
#'
#' **Start Belief**
#'
#' The initial belief state of the agent is a distribution over the states. It is used to calculate the
#' total expected cumulative reward printed with the solved model. The function [reward()] can be
#' used to calculate rewards for any belief.
#'
#' Some methods use this belief to decide which belief states to explore (e.g.,
#' the finite grid method).
#'
#' Options to specify the start belief state are:
#'
#' * A probability distribution over the states. That is, a vector
#'   of \eqn{|S|} probabilities, that add up to \eqn{1}.
#'
#'* The string `"uniform"` for a uniform
#'   distribution over all states.
#'* An integer in the range \eqn{1} to \eqn{n} to specify the index of a single starting state.
#'* A string specifying the name of a single starting state.
#'
#' The default initial belief is a uniform
#' distribution over all states.
#'
#' **Time-dependent POMDPs**
#'
#' Time dependence of transition probabilities, observation probabilities and
#' reward structure can be modeled by considering a set of episodes
#' representing epoch with the same settings. The length of each episode is
#' specified as a vector for `horizon`, where the length is the number of
#' episodes and each value is the length of the episode in epochs. Transition
#' probabilities, observation probabilities and/or reward structure can contain
#' a list with the values for each episode. See [solve_POMDP()] for
#' more details and an example.
#'
#' @family POMDP
#'
#' @param states a character vector specifying the names of the states. Note that
#' state names have to start with a letter.
#' @param actions a character vector specifying the names of the available actions.
#' Note that action names have to start with a letter.
#' @param observations a character vector specifying the names of the
#' observations. Note that observation names have to start with a letter.
#' @param transition_prob Specifies action-dependent transition probabilities
#' between states.  See Details section.
#' @param observation_prob Specifies the probability that an action/state
#' combination produces an observation.  See Details section.
#' @param reward Specifies the rewards structure dependent on action, states
#' and observations.  See Details section.
#' @param discount numeric; discount factor between 0 and 1.
#' @param horizon numeric; Number of epochs. `Inf` specifies an infinite
#' horizon.
#' @param terminal_values a vector with the terminal values for each state or a
#' matrix specifying the terminal rewards via a terminal value function (e.g.,
#' the alpha component produced by solve_POMDP).  A single 0 specifies that all
#' terminal values are zero.
#' @param start Specifies the initial belief state of the agent. A vector with the
#' probability for each state is supplied. Also the string `'uniform'`
#' (default) can be used.  The belief is used to calculate the total expected cumulative
#' reward. It is also used by some solvers. See Details section for more
#' information.
#' @param name a string to identify the POMDP problem.
#' @param action,start.state,end.state,observation,probability,value Values
#' used in the helper functions `O_()`, `R_()`, and `T_()` to
#' create an entry for `observation_prob`, `reward`, or
#' `transition_prob` above, respectively. The default value `'*"'`
#' matches any action/state/observation.
#'
#' @return The function returns an object of class POMDP which is list of the model specification.
#' [solve_POMDP()] reads the object and adds a list element named
#' `'solution'`.
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @references
#' pomdp-solve website: \url{http://www.pomdp.org}
#' @examples
#' ## Defining the Tiger Problem (it is also available via data(Tiger), see ? Tiger)
#'
#' Tiger <- POMDP(
#'   name = "Tiger Problem",
#'   discount = 0.75,
#'   states = c("tiger-left" , "tiger-right"),
#'   actions = c("listen", "open-left", "open-right"),
#'   observations = c("tiger-left", "tiger-right"),
#'   start = "uniform",
#'
#'   transition_prob = list(
#'     "listen" =     "identity",
#'     "open-left" =  "uniform",
#'     "open-right" = "uniform"
#'   ),
#'
#'   observation_prob = list(
#'     "listen" = rbind(c(0.85, 0.15),
#'                      c(0.15, 0.85)),
#'     "open-left" =  "uniform",
#'     "open-right" = "uniform"
#'   ),
#'
#'   # the reward helper expects: action, start.state, end.state, observation, value
#'   # missing arguments default to '*' matching any value.
#'   reward = rbind(
#'     R_("listen",                    v =   -1),
#'     R_("open-left",  "tiger-left",  v = -100),
#'     R_("open-left",  "tiger-right", v =   10),
#'     R_("open-right", "tiger-left",  v =   10),
#'     R_("open-right", "tiger-right", v = -100)
#'   )
#' )
#'
#' Tiger
#'
#' # Defining the Tiger problem using functions
#'
#' trans_f <- function(action, start.state, end.state) {
#'   if(action == 'listen')
#'     if(end.state == start.state) return(1)
#'     else return(0)
#'
#'   return(1/2) ### all other actions have a uniform distribution
#' }
#'
#' obs_f <- function(action, end.state, observation) {
#'   if(action == 'listen')
#'     if(end.state == observation) return(0.85)
#'   else return(0.15)
#'
#'   return(1/2)
#' }
#'
#' rew_f <- function(action, start.state, end.state, observation) {
#'   if(action == 'listen') return(-1)
#'   if(action == 'open-left' && start.state == 'tiger-left') return(-100)
#'   if(action == 'open-left' && start.state == 'tiger-right') return(10)
#'   if(action == 'open-right' && start.state == 'tiger-left') return(10)
#'   if(action == 'open-right' && start.state == 'tiger-right') return(-100)
#'   stop('Not possible')
#' }
#'
#' Tiger_func <- POMDP(
#'   name = "Tiger Problem",
#'   discount = 0.75,
#'   states = c("tiger-left" , "tiger-right"),
#'   actions = c("listen", "open-left", "open-right"),
#'   observations = c("tiger-left", "tiger-right"),
#'   start = "uniform",
#'   transition_prob = trans_f,
#'   observation_prob = obs_f,
#'   reward = rew_f
#' )
#'
#' Tiger_func
#' @export
POMDP <- function(states,
  actions,
  observations,
  transition_prob,
  observation_prob,
  reward,
  discount = .9,
  horizon = Inf,
  terminal_values = NULL,
  start = "uniform",
  name = NA) {
  ### unsolved pomdp model
  x <- list(
    name = name,
    discount = discount,
    horizon = horizon,
    states = states,
    actions = actions,
    observations = observations,
    transition_prob = transition_prob,
    observation_prob = observation_prob,
    reward = reward,
    start = start,
    terminal_values = terminal_values
  )
  
  class(x) <- c("POMDP", "list")
  check_and_fix_MDP(x)
}




# make sure the definition is complete and everything is in the right order and the right factors
check_and_fix_MDP <- function(x) {
  ### TODO: check function needs to be used!
  ### TODO: keep functions. For now we expand functions into matrices
  check_func <- function(x, func, name) {
    req_formals <- head(names(formals(func)),-1)
    if (!identical(names(formals(x)), req_formals))
      stop(name,
        " function needs formal arguments: ",
        paste(sQuote(req_formals), collapse = ", "))
  }
  
  check_df <- function(x, field, func) {
    req_columns <- names(formals(func))
    if (is.null(colnames(field)))
      colnames(field) <- req_columns
    
    if (!identical(colnames(field), req_columns))
      stop(
        "The ",
        deparse(substitute(field)),
        " data.frame needs columns named: ",
        paste(sQuote(req_columns), collapse = ", ")
      )
    
    # convert * to NA
    field[field == '*'] <- NA
    field <- type.convert(field, as.is = TRUE)
    
    for (i in grep("action", colnames(field))) {
      if (is.numeric(field[[i]]))
        field[[i]] <- x$actions[field[[i]]]
      field[[i]] <- factor(field[[i]], levels = x$actions)
    }
    
    for (i in grep("state", colnames(field))) {
      if (is.numeric(field[[i]]))
        field[[i]] <- x$states[field[[i]]]
      field[[i]] <- factor(field[[i]], levels = x$states)
    }
    for (i in grep("observation", colnames(field))) {
      if (is.numeric(field[[i]]))
        field[[i]] <- x$observations[field[[i]]]
      field[[i]] <- factor(field[[i]], levels = x$observations)
    }
    
    field
  }
  
  ### do the checking
  
  # expand states, actions and observations
  if (is.numeric(x$states) &&
      length(x$states) == 1L)
    x$states <- paste0("s", seq_len(x$states))
  
  if (is.numeric(x$actions) &&
      length(x$actions) == 1L)
    x$actions <- paste0("a", seq_len(x$actions))
  
  if (inherits(x, "POMDP")) {
    if (is.numeric(x$observations) &&
        length(x$observations) == 1L)
      x$observations <- paste0("o", seq_len(x$observations))
  }
  
  x$discount <- as.numeric(x$discount)
  if (length(x$discount) != 1L ||
      x$discount < 0 || x$discount > 1)
    stop("discount has to be a single value in the range [0,1].")
  
  if (is.null(x$horizon))
    x$horizon <- Inf
  x$horizon <- as.numeric(x$horizon)
  if (any(x$horizon != floor(x$horizon)))
    stop("horizon needs to be an integer.")
  
  
  # start
  if (is.numeric(x$start) &&
      length(x$start) == length(x$states)) {
    if (!sum1(x$start))
      stop("The start probability vector does not add up to 1.")
    if (is.null(names(x$start)))
      names(x$start) <- x$states
    else
      x$start <- x$start[x$states]
  }
  if (any(is.na(x$start)))
    stop("start containes undefined start states.")
  if (is.character(x$start)) {
    if (!(identical(x$start, "uniform") || all(x$start %in% x$states)))
      stop(
        "when using characters for start, then it needs to be the keyword 'uniform' or a set of start states."
      )
  }
  
  ## TODO: check terminal_values
  
  if ((is.null(x$transition_prob) ||
      (inherits(x, "POMDP") &&
          is.null(x$observation_prob)) ||
      is.null(x$reward)) && is.null(x$problem))
    stop(
      "transition_prob, observation_prob or reward can only miss if the field problem is available!"
    )
  
  if (!is.null(x$transition_prob)) {
    # if we have matrices then check and add names
    #check_func(x$transition_prob, T_, "transition_prob")
    if (is.function(x$transition_prob))
      x$transition_prob <- transition_matrix(x)
    else if (is.data.frame(x$transition_prob))
      x$transition_prob <- check_df(x, x$transition_prob, T_)
    else {
      if (is.null(names(x$transition_prob)))
        names(x$transition_prob) <- x$actions
      for (a in x$actions) {
        if (is.null(x$transition_prob[[a]]))
          stop("transition_prob for action ", a, " is missing!")
        if (is.matrix(x$transition_prob[[a]])) {
          if (!identical(dim(x$transition_prob[[a]]), c(length(x$states), length(x$states))))
            stop("transition_prob matrix for action ",
              a,
              ": has not the right dimensions!")
          if (!sum1(x$transition_prob[[a]]))
            stop("transition_prob matrix for action ",
              a,
              ": rows do not add up to 1!")
          if (is.null(dimnames(x$transition_prob[[a]])))
            dimnames(x$transition_prob[[a]]) <-
              list(x$states, x$states)
          else
            x$transition_prob[[a]][x$states, x$states]
        }
      }
    }
  }
  if (!is.null(x$reward)) {
    #check_func(reward, R_, "reward")
    if (is.function(x$reward))
      x$reward <- reward_matrix(x)
    
    if (is.data.frame(x$reward))
      x$reward <- check_df(x, x$reward, R_)
    else {
      if (is.null(names(x$reward)))
        names(x$reward) <- x$actions
      for (a in x$actions) {
        if (is.null(x$reward[[a]]))
          stop("reward for action ", a, " is missing!")
        for (s in x$states) {
          if (is.null(x$reward[[a]][[s]]))
            stop("reward for action ",
              a,
              " and state ",
              s,
              " is missing!")
          if (is.matrix(x$reward[[a]][[s]])) {
            if (!identical(dim(x$reward[[a]][[s]]), c(length(x$states), length(x$observations))))
              stop(
                "reward matrix for action ",
                a,
                " and start.state ",
                s,
                ": has not the right dimensions!"
              )
            if (is.null(dimnames(x$reward[[a]][[s]])))
              dimnames(x$reward[[a]][[s]]) <-
                list(x$states, x$observations)
            else
              x$reward[[a]][[s]][x$states, x$observations]
          }
        }
      }
    }
    
    if (inherits(x, "POMDP") && !is.null(x$observation_prob)) {
      #check_func(observation_prob, O_, "observation_prob")
      if (is.function(x$observation_prob))
        x$observation_prob <- observation_matrix(x)
      
      if (is.data.frame(x$observation_prob))
        x$observation_prob <-
          check_df(x, x$observation_prob, O_)
      else {
        if (is.null(names(x$observation_prob)))
          names(x$observation_prob) <- x$actions
        for (a in x$actions) {
          if (is.null(x$observation_prob[[a]]))
            stop("observation_prob for action ", a, " is missing!")
          if (is.matrix(x$observation_prob[[a]])) {
            if (!identical(dim(x$observation_prob[[a]]), c(length(x$states), length(x$observations))))
              stop("observation_prob matrix for action ",
                a,
                ": has not the right dimensions!")
            if (!all(rowSums(x$observation_prob[[a]]) == 1))
              stop("observation_prob matrix for action ",
                a,
                ": rows do not add up to 1!")
            
            if (is.null(dimnames(x$observation_prob[[a]])))
              dimnames(x$observation_prob[[a]]) <-
                list(x$states, x$observations)
            else
              x$observation_prob[[a]][x$states, x$observations]
          }
        }
      }
    }
  }
  x
}

#' @export
print.POMDP <- function(x, ...) {
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
    "  Size: %d states / %d actions / %d obs.\n",
    length(x$states),
    length(x$actions),
    length(x$observations)
  ))
  
  if (.solved_POMDP(x))
    writeLines(c(
      "  Solved:",
      sprintf("    Method: %s",
        x$solution$method),
      sprintf("    Solution converged: %s",
        x$solution$converged),
      sprintf(
        "    Total expected reward: %f",
        x$solution$total_expected_reward
      )
    ))
  
  writeLines(strwrap(
    paste("List components:", paste(sQuote(names(
      x
    )), collapse = ", "), "\n"),
    indent = 2,
    exdent = 4
  ))
}


# check if x is a solved POMDP
.solved_POMDP <- function(x, stop = FALSE) {
  if (!inherits(x, "POMDP"))
    stop("x needs to be a POMDP object!")
  
  solved <- !is.null(x$solution)
  if (stop && !solved)
    stop("x needs to be a solved POMDP. Use solve_POMDP() first.")
  
  solved
}

.timedependent_POMDP <- function(x)
  ! is.null(x$horizon) && length(x$horizon) > 1L


# get pg and alpha for a epoch
.get_pg_index <- function(model, epoch) {
  #.solved_POMDP(model, stop = TRUE)
  
  epoch <- as.integer(epoch)
  if (epoch < 1L)
    stop("Epoch has to be >= 1")
  
  ### (converged) infinite horizon POMDPs. We ignore epoch.
  if (length(model$solution$pg) == 1L)
    return(1L)
  
  ### regular epoch for finite/infinite horizon case
  if (epoch > length(model$solution$pg))
    stop("POMDP model has only solutions for ",
      length(model$solution$pg),
      " epochs!")
  
  return(epoch)
}

.get_pg <-
  function(model, epoch)
    model$solution$pg[[.get_pg_index(model, epoch)]]

.get_alpha <-
  function(model, epoch)
    model$solution$alpha[[.get_pg_index(model, epoch)]]

#' @rdname POMDP
#' @export
O_ <-
  function(action = "*",
    end.state = "*",
    observation = "*",
    probability)
    data.frame(
      action = action,
      end.state = end.state,
      observation = observation,
      probability = as.numeric(probability),
      stringsAsFactors = FALSE
    )

#' @rdname POMDP
#' @export
T_ <-
  function(action = "*",
    start.state = "*",
    end.state = "*",
    probability)
    data.frame(
      action = action,
      start.state = start.state,
      end.state = end.state,
      probability = as.numeric(probability),
      stringsAsFactors = FALSE
    )

#' @rdname POMDP
#' @export
R_ <-
  function(action = "*",
    start.state = "*",
    end.state = "*",
    observation = "*",
    value)
    data.frame(
      action = action,
      start.state = start.state,
      end.state = end.state,
      observation = observation,
      value = as.numeric(value),
      stringsAsFactors = FALSE
    )
