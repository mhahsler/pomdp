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
#' Note that the observation model is in the literature
#' often also denoted by the letter \eqn{Z}.
#'
#' **Names used for mathematical symbols in code**
#'
#' * \eqn{S, s, s'}: `'states', start.state', 'end.state'`
#' * \eqn{A, a}: `'actions', 'action'`
#' * \eqn{\Omega, o}: `'observations', 'observation'`
#'
#' State names, actions and observations can be specified as strings or index numbers
#' (e.g., `start.state` can be specified as the index of the state in `states`).
#' For the specification as data.frames below, `NA` can be used to mean
#' any  `start.state`, `end.state`, `action` or `observation`. Note that some POMDP solvers and the POMDP
#' file format use `'*'` for this purpose.
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
#'   frame. Probabilities can be specified multiple times and the definition that
#'   appears last in the data.frame will take affect.
#'
#' * A named list of matrices, one for each action. Each matrix is square with
#'   rows representing start states \eqn{s} and columns representing end states \eqn{s'}.
#'   Instead of a matrix, also the strings `'identity'` or `'uniform'` can be specified.
#'
#' * A function with the same arguments are `T_()`, but no default values
#'   that returns the transition probability.
#'
#' **Specification of observation probabilities: \eqn{O(o | a, s')}**
#'
#' The POMDP specifies the probability for each observation \eqn{o} given an
#' action \eqn{a} and that the system transitioned to the end state
#' \eqn{s'}. These probabilities can be specified in the
#' following ways:
#'
#' * A data frame with columns named exactly like the arguments of `O_()`.
#'   You can use `rbind()`
#'   with helper function `O_()` to create this data frame. Probabilities can be
#'   specified multiple times and the definition that
#'   appears last in the data.frame will take affect.
#'
#' * A named list of matrices, one for each action. Each matrix has
#'   rows representing end states \eqn{s'} and columns representing an observation \eqn{o}.
#'   Instead of a matrix, also the strings `'identity'` or `'uniform'` can be specified.
#'
#' * A function with the same arguments are `O_()`, but no default values
#'   that returns the observation probability.
#'
#' **Specification of the reward function: \eqn{R(a, s, s', o)}**
#'
#' The reward function can be specified in the following
#' ways:
#'
#' * A data frame with columns named exactly like the arguments of `R_()`.
#'   You can use `rbind()`
#'   with helper function `R_()` to create this data frame. Rewards can be specified
#'   multiple times and the definition that
#'   appears last in the data.frame will take affect.
#'
#' * A list of lists. The list levels are `'action'` and `'start.state'`. The list elements
#'   are matrices with
#'   rows representing end states \eqn{s'} and columns representing an observation \eqn{o}.
#'
#' * A function with the same arguments are `R_()`, but no default values
#'   that returns the reward.
#'   
#' To avoid overflow problems with rewards, reward values should stay well within the 
#' range of 
#' `[-1e10, +1e10]`. `-Inf` can be used as the reward for unavailable actions and
#' will be translated into a large negative reward for solvers that only support
#' finite reward values. 
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
#' **Convergence**
#'
#' A infinite-horizon POMDP needs to converge to provide a valid value
#' function and policy.
#'
#' A finite-horizon POMDP may also converging to a infinite horizon solution
#' if the horizon is long enough.
#'
#' **Time-dependent POMDPs**
#'
#' Time dependence of transition probabilities, observation probabilities and
#' reward structure can be modeled by considering a set of **episodes**
#' representing **epoch** with the same settings. The length of each episode is
#' specified as a vector for `horizon`, where the length is the number of
#' episodes and each value is the length of the episode in epochs. Transition
#' probabilities, observation probabilities and/or reward structure can contain
#' a list with the values for each episode. The helper function `epoch_to_episode()` converts
#' an epoch to the episode it belongs to.
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
#' the alpha component produced by `solve_POMDP()`).  A single 0 specifies that all
#' terminal values are zero.
#' @param start Specifies the initial belief state of the agent. A vector with the
#' probability for each state is supplied. Also the string `'uniform'`
#' (default) can be used.  The belief is used to calculate the total expected cumulative
#' reward. It is also used by some solvers. See Details section for more
#' information.
#' @param normalize logical; should the description be normalized for faster access (see [normalize_POMDP()])?
#' @param info A list with additional information.
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
#'   # missing arguments default to NA which matches any value (often denoted as * in POMDPs).
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
#' ### Defining the Tiger problem using functions
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
#'
#' # Defining a Time-dependent version of the Tiger Problem called Scared Tiger
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
                  normalize = TRUE,
                  info = NULL,
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
    terminal_values = terminal_values,
    info = info
  )
  
  class(x) <- c("POMDP", "list")
  x <- check_and_fix_MDP(x)
  
  if (normalize)
    x <- normalize_POMDP(x, sparse = TRUE)
  
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
    "  Size: %d states / %d actions / %d obs.",
    length(x$states),
    length(x$actions),
    length(x$observations)
  ))
  
  writeLines(paste0("  Start: ", shorten(paste(
    x$start, collapse = ", "
  ), n = -10L)))
  
  if (is_solved_POMDP(x))
    writeLines(c(
      "  Solved:",
      sprintf("    Method: %s",
              sQuote(x$solution$method)),
      sprintf("    Solution converged: %s",
              x$solution$converged),
      sprintf("    # of alpha vectors: %i",
              sum(sapply(
                x$solution$alpha, nrow
              ))),
      sprintf(
        "    Total expected reward: %f",
        x$solution$total_expected_reward
      ),
      ""
    ))
  else
    writeLines(c("  Solved: FALSE\n"))
  
  
  writeLines(strwrap(
    paste("List components:", paste(sQuote(names(
      x
    )), collapse = ", "), "\n"),
    indent = 2,
    exdent = 4
  ))
}

# check if x is a solved POMDP
#' @rdname POMDP
#' @param x a POMDP.
#' @param stop logical; stop with an error.
#' @export
is_solved_POMDP <- function(x, stop = FALSE, message = "") {
  if (!inherits(x, "POMDP"))
    stop("x needs to be a POMDP object!")
  
  solved <- !is.null(x$solution)
  if (stop && !solved)
    stop("x needs to be a solved POMDP. Use solve_POMDP() first.",
         message,
         call. = FALSE)
  
  solved
}

#' @rdname POMDP
#' @export
is_timedependent_POMDP <- function(x)
  ! is.null(x$horizon) && length(x$horizon) > 1L


# is a field time-dependent? For time-dependence we have a list of
# matrices/data.frames or for observation_prob we have a list of a list
.is_timedependent_field <- function(x, field) {
  field <-
    match.arg(field, c("transition_prob", "observation_prob", "reward"))
  m <- x[[field]]
  if (is.null(m))
    stop("Field ", field, " does not exist.")
  
  # it is a list. time dependent is a list (episodes) of lists
  if (!is.list(m) || is.data.frame(m))
    return(FALSE)
  if (!is.list(m[[1]]))
    return(FALSE)
  
  # time dependent reward is a list of lists of lists
  if (field == "reward" && !is.list(m[[1]][[1]]))
    return(FALSE)
  
  if (length(m) != length(x$horizon))
    stop(
      "Inconsistent POMDP specification. Field ",
      field,
      " does not contain data for the appropriate number of episodes."
    )
  
  TRUE
}

#' @rdname POMDP
#' @param epoch integer; an epoch that should be converted to the corresponding episode in a time-dependent
#' POMDP.
#' @export
epoch_to_episode <- function(x, epoch) {
  if (is.null(epoch))
    return(1L)
  
  episode <- which(epoch <= cumsum(x$horizon))[1]
  if (is.na(episode))
    stop("Epoch does not exist")
  
  episode
}

#' @rdname POMDP
#' @param message a error message to be displayed displayed
#' @export
is_converged_POMDP <- function(x, stop = FALSE, message = "") {
  is_solved_POMDP(x, stop = stop)
  
  converged <- x$solution$converged && length(x$solution$pg) == 1L
  
  if (stop && !converged)
    stop("POMDP solution has not converged. ", message, call. = FALSE)
  
  converged
}

# get pg and alpha for a epoch
.get_pg_index <- function(model, epoch) {
  #is_solved_POMDP(model, stop = TRUE)
  
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
  function(action = NA,
           end.state = NA,
           observation = NA,
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
  function(action = NA,
           start.state = NA,
           end.state = NA,
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
  function(action = NA,
           start.state = NA,
           end.state = NA,
           observation = NA,
           value)
    data.frame(
      action = action,
      start.state = start.state,
      end.state = end.state,
      observation = observation,
      value = as.numeric(value),
      stringsAsFactors = FALSE
    )