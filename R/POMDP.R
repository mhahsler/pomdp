#' Define a POMDP Problem
#'
#' Defines all the elements of a POMDP problem including the discount rate, the
#' set of states, the set of actions, the set of observations, the transition
#' probabilities, the observation probabilities, and rewards.
#'
#' POMDP problems can be solved using \code{\link{solve_POMDP}}.  More details
#' about the available specifications can be found in [1].
#'
#' In the following we use the following notation. The POMDP is a 7-duple
#' \eqn{(S,A,T,R, \Omega ,O, \gamma)}.  \eqn{S} is the set of states; \eqn{A}
#' is the set of actions; \eqn{T} are the conditional transition probabilities
#' between states; \eqn{R} is the reward function; \eqn{\Omega} is the set of
#' observations; \eqn{O} are the conditional observation probabilities; and
#' \eqn{\gamma} is the discount factor. We will use lower case letters to
#' represent a member of a set, e.g., \eqn{s} is a specific state. To refer to
#' the size of a set we will use cardinality, e.g., the number of actions is
#' \eqn{|A|}.
#'
#' \bold{Specification of transition probabilities}
#'
#' Transition probability to transition to state \eqn{s'} from \eqn{s} given
#' action \eqn{a} is \eqn{T(s' | s, a)}. The transition probabilities can be
#' specified in the following ways:
#'
#' \itemize{
#' \item A data frame with 4 columns, where the columns specify
#' action \eqn{a}, start.state \eqn{s}, end.state \eqn{s'} and the transition
#' probability \eqn{T(s' | s, a)}, respectively. The first 3 columns can be
#' either character (the name of the action or state) or integer indices.  You
#' can use \code{rbind()} with helper function \code{T_()} to create this data
#' frame.
#'
#' \item A named list of \eqn{|A|} matrices.  Each matrix is square of size
#' \eqn{|S| \times |S|}{|S| x |S|}. Instead of a matrix, also the strings
#' \code{"identity"} or \code{"uniform"} can be specified.
#' }
#'
#' \bold{Specification of observation probabilities}
#'
#' The POMDP specifies the probability for each observation \eqn{o} given an
#' action \eqn{a} and that the system transitioned to a specific state
#' \eqn{s'}, \eqn{O(o | s', a)}. These probabilities can be specified in the
#' following ways:
#'
#' \itemize{ \item A data frame with 4 columns, where the columns specify the
#' action \eqn{a}, the end.state \eqn{s'}, the observation \eqn{o} and the
#' probability \eqn{O(o | s', a)}, respectively. The first 3 columns could be
#' either character (the name of the action, state, or observation), integer
#' indices, or they can be \code{"*"} to indicate that the observation
#' probability applies to all actions or states.  You can use \code{rbind()}
#' with helper function \code{O_()} to create this data frame.
#'
#' \item A named list of \eqn{|A|} matrices. Each matrix is of size \eqn{|S|
#' \times |\Omega|}{|S| x |\Omega|}.  The name of each matrix is the action it
#' applies to.  Instead of a matrix, also the string \code{"uniform"} can be
#' specified.}
#'
#' \bold{Specification of the reward function}
#'
#' The reward function \eqn{R(s, s', o, a)} can be specified in the following
#' ways:
#'
#' \itemize{
#' \item a data frame with 5 columns, where the columns specify
#' action \eqn{a}, start.state \eqn{s}, end.state \eqn{s'}, observation \eqn{o}
#' and the associated reward \eqn{R(s, s', a)}, respectively. The first 4
#' columns could be either character (names of the action, states, or
#' observation), integer indices, or they can be \code{"*"} to indicate that
#' the reward applies to all transitions.  Use \code{rbind()} with helper
#' function \code{R_()} to create this data frame.
#'
#' \item a named list of \eqn{|A|} lists. Each list contains \eqn{|S|} named
#' matrices representing the start states \eqn{s}. Each matrix is of size
#' \eqn{|S| \times |\Omega|}{|S| x |\Omega|}, representing the end states
#' \eqn{s'} and observations.
#' }
#'
#' \bold{Start Belief}
#'
#' This belief is used to calculate the total expected cumulative reward
#' printed with the solved model.  The function \code{\link{reward}} can be
#' used to calculate rewards for any belief.
#'
#' Some methods use this belief to decide which belief states to explore (e.g.,
#' the finite grid method).  The default initial belief is a uniform
#' distribution over all states. No initial belief state can be used by setting
#' \code{start = NULL}.
#'
#' Options to specify the start belief state are: \itemize{ \item a probability
#' distribution over the states. That is, a vector of \eqn{|S|} probabilities,
#' that add up to \eqn{1}.  \item the string \code{"uniform"} for a uniform
#' distribution over all states.  \item an integer in the range \eqn{1} to
#' \eqn{n} to specify the index of a single starting state.  \item a string
#' specifying the name of a single starting state.  }
#'
#' \bold{Time-dependent POMDPs}
#'
#' Time dependence of transition probabilities, observation probabilities and
#' reward structure can be modeled by considering a set of episodes
#' representing epoch with the same settings. The length of each episode is
#' specified as a vector for \code{horizon}, where the length is the number of
#' episodes and each value is the length of the episode in epochs. Transition
#' probabilities, observation probabilities and/or reward structure can contain
#' a list with the values for each episode. See \code{\link{solve_POMDP}} for
#' more details and an example.
#'
#' @aliases POMDP O_ R_ T_
#' @param states a character vector specifying the names of the states.
#' @param actions a character vector specifying the names of the available
#' actions.
#' @param observations a character vector specifying the names of the
#' observations.
#' @param transition_prob Specifies action-dependent transition probabilities
#' between states.  See Details section.
#' @param observation_prob Specifies the probability that an action/state
#' combination produces an observation.  See Details section.
#' @param reward Specifies the rewards structure dependent on action, states
#' and observations.  See Details section.
#' @param discount numeric; discount factor between 0 and 1.
#' @param horizon numeric; Number of epochs. \code{Inf} specifies an infinite
#' horizon.
#' @param terminal_values a vector with the terminal values for each state or a
#' matrix specifying the terminal rewards via a terminal value function (e.g.,
#' the alpha component produced by solve_POMDP).  A single 0 specifies that all
#' terminal values are zero.
#' @param start Specifies the initial probabilities for each state (i.e., the
#' initial belief), typically as a vector or the string \code{"uniform"}
#' (default).  This belief is used to calculate the total expected cumulative
#' reward. It is also used by some solvers. See Details section for more
#' information.
#' @param max logical; is this a maximization problem (maximize reward) or a
#' minimization (minimize cost specified in \code{reward})?
#' @param name a string to identify the POMDP problem.
#' @param action,start.state,end.state,observation,probability,value Values
#' used in the helper functions \code{O_()}, \code{R_()}, and \code{T_()} to
#' create an entry for \code{observation_prob}, \code{reward}, or
#' \code{transistion_prob} above, respectively. The default value \code{"*"}
#' matches any action/state/observation.
#' @return The function returns an object of class POMDP which is list with an
#' element called \code{model} containing a list with the model specification.
#' \code{solve_POMDP} reads the object and adds a list element called
#' \code{solution}.
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @seealso \code{\link{solve_POMDP}}
#' @references [1] For further details on how the POMDP solver utilized in this
#' R package works check the following website: \url{http://www.pomdp.org}
#' @examples
#'
#' ## The Tiger Problem
#'
#' Tiger <- POMDP(
#'   name = "Tiger Problem",
#'
#'   discount = 0.75,
#'
#'   states = c("tiger-left" , "tiger-right"),
#'   actions = c("listen", "open-left", "open-right"),
#'   observations = c("tiger-left", "tiger-right"),
#'
#'   start = "uniform",
#'
#'   transition_prob = list(
#'     "listen" =     "identity",
#'     "open-left" =  "uniform",
#'     "open-right" = "uniform"),
#'
#'   observation_prob = list(
#'     "listen" = rbind(c(0.85, 0.15),
#'                      c(0.15, 0.85)),
#'     "open-left" =  "uniform",
#'     "open-right" = "uniform"),
#'
#'   # the reward helper expects: action, start.state, end.state, observation, value
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
#' Tiger$model
#'
#' @export
POMDP <- function(states,
  actions,
  observations,
  transition_prob,
  observation_prob,
  reward,
  discount = .9,
  horizon = Inf,
  terminal_values = 0,
  start = "uniform",
  max = TRUE,
  name = NA) {
  ### FIXME: Check the values!
  
  # discount should be a number in [0,1]
  # states should be a vector of strings
  # actions should be a vector of strings
  # observations should be a vector of strings
  # start should be either a vector of n numbers each in [0,1] that add up to 1 where n is the number of states
  # or the word "uniform", or a single number in 1 to n, or the name of a single state, or the names of a subset of states
  
  ### add names to start
  if (is.numeric(start) && length(start) == length(states)) {
    if (is.null(names(start)))
      names(start) <- states
    else
      start <- start[states]
  }
  
  # transition_prob is either a list consisting of m matrices where m is the number of actions
  # or a data frame with 4 columns
  
  ### add names to transition probabilities
  for (a in names(transition_prob)) {
    if (is.matrix(transition_prob[[a]]))
      dimnames(transition_prob[[a]]) <- list(states, states)
  }
  
  
  # observation_prob is either a list consisting of m matrices where m is the number of actions
  # or a data frame with 4 columns
  # reward should be either a matrix of size mxn where n is the number of states or
  # a data frame with 5 columns
  # grid_size is an integer
  
  ### unsolved pomdp model
  structure(list(model = structure(
    list(
      name = name,
      discount = discount,
      horizon = horizon,
      states = as.character(states),
      actions = as.character(actions),
      observations = as.character(observations),
      transition_prob = transition_prob,
      observation_prob = observation_prob,
      reward = reward,
      start = start,
      terminal_values = terminal_values,
      max = max
    ),
    class = "POMDP_model"
  )), class = "POMDP")
}

#' @export
print.POMDP <- function(x, ...) {
  if (is.null(x$solution))
    cat(
      "Unsolved POMDP model:",
      x$model$name,
      "\n",
      "\thorizon:",
      paste(x$model$horizon, collapse = "+"),
      "\n",
      if (!is.null(x$model$horizon) &&
          length(x$model$horizon) > 1)
        paste("\ttime-dependent:", length(x$model$horizon), "episodes\n")
    )
  else
    cat(
      "Solved POMDP model:",
      x$model$name,
      "\n",
      "\tsolution method:",
      x$solution$method,
      "\n",
      "\thorizon:",
      x$solution$horizon,
      "\n",
      if (!is.null(x$model$horizon) &&
          length(x$model$horizon) > 1)
        paste("\ttime-dependent:", length(x$model$horizon), "episodes\n")
      else
        "",
      "\tconverged:",
      x$solution$converged,
      "\n",
      "\ttotal expected reward (for start probabilities):",
      x$solution$total_expected_reward,
      "\n"
    )
}

#' @export
print.POMDP_model <- function(x, ...) {
  cat("POMDP model:", x$name, "\n\n")
  if (!is.null(x$problem)) {
    cat("Model specification from POMDP file\n\n")
    cat("--------------- Start ---------------\n")
    cat(x$problem, sep = "\n")
    cat("---------------- End ----------------\n\n")
    x$problem <- NULL
  }
  print(unclass(x))
}


# check if x is a solved POMDP
.solved_POMDP <- function(x) {
  if (!inherits(x, "POMDP"))
    stop("x needs to be a POMDP object!")
  if (is.null(x$solution))
    stop("x needs to be a solved POMDP. Use solve_POMDP() first.")
}

.timedependent_POMDP <- function(x) {
  !is.null(x$model$horizon) && length(x$model$horizon) > 1L
}

# get pg and alpha for a epoch
.get_pg_index <- function(model, epoch) {
  .solved_POMDP(model)
  
  if (epoch < 1)
    stop("Epoch has to be >= 1")
  
  h <- model$solution$horizon
  l <- length(model$solution$pg)
  
  if (epoch > h)
    stop("POMDP model was only solved for ", h, " epochs!")
  
  ### (converged) infinite horizon POMDPs
  if (is.infinite(h))
    epoch <- 1L
  
  ### converged finite horizon model
  else {
    if (epoch <= h - l)
      epoch <- 1L
    else
      epoch <- epoch - (h - l)
  }
  
  epoch
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
      probability = probability,
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
      probability = probability,
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
      value = value,
      stringsAsFactors = FALSE
    )
