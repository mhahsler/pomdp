#' Define a POMDP Problem
#'
#' Defines all the elements of a POMDP problem including the discount rate, the
#' set of states, the set of actions, the set of observations, the transition
#' probabilities, the observation probabilities, and rewards.
#'
#' POMDP problems can be solved using [solve_POMDP()].
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
#' For the specification as data.frames below, `'*'` can be used to mean
#' any  `start.state`, `end.state`, `action` or `observation`.
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
#' This belief is used to calculate the total expected cumulative reward
#' printed with the solved model.  The function [reward()] can be
#' used to calculate rewards for any belief.
#'
#' Some methods use this belief to decide which belief states to explore (e.g.,
#' the finite grid method).  The default initial belief is a uniform
#' distribution over all states. No initial belief state can be used by setting
#' `start = NULL`.
#'
#' Options to specify the start belief state are:
#'
#' * A probability distribution over the states. That is, a vector
#'   of \eqn{|S|} probabilities, that add up to \eqn{1}.
#'
#'* The string `"uniform"` for a uniform
#'   distribution over all states.
#'* An integer in the range \eqn{1} to \eqn{n} to specify the index of a single starting state.
#'* a string specifying the name of a single starting state.
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
#' @param start Specifies the initial probabilities for each state (i.e., the
#' initial belief), typically as a vector or the string `'uniform'`
#' (default).  This belief is used to calculate the total expected cumulative
#' reward. It is also used by some solvers. See Details section for more
#' information.
#' @param max logical; is this a maximization problem (maximize reward) or a
#' minimization (minimize cost specified in `reward`)?
#' @param name a string to identify the POMDP problem.
#' @param action,start.state,end.state,observation,probability,value Values
#' used in the helper functions `O_()`, `R_()`, and `T_()` to
#' create an entry for `observation_prob`, `reward`, or
#' `transistion_prob` above, respectively. The default value `'*"'`
#' matches any action/state/observation.
#'
#' @return The function returns an object of class POMDP which is list with an
#' element called `'model'` containing a list with the model specification.
#' [solve_POMDP()] reads the object and adds a list element named
#' `'solution'`.
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @seealso [solve_POMDP()]
#' @references
#' pomdp-solve website: \url{http://www.pomdp.org}
#' @examples
#' ## Defining the Tiger Problem (it is also avilable via data(Tiger))
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
#' Tiger$model
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
#' Tiger_func$model
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
  ### unsolved pomdp model
  x <- list(
    model = list(
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
      max = max
    )
  )
  
  class(x) <- "POMDP"
  check_and_fix_MDP(x)
}

check_func <- function(x, func, name) {
  if (is.function(x)) {
    req_formals <- head(names(formals(func)),-1)
    if (!identical(names(formals(x)), req_formals))
      stop(name,
        " function needs formal arguments: ",
        paste(sQuote(req_formals), collapse = ", "))
  }
}

check_df <- function(x, func, name) {
  if (is.data.frame(x)) {
    req_columns <- names(formals(func))
    if (!identical(colnames(x), req_columns))
      stop("The ",
        name,
        " data.frame needs columns named: ",
        paste(sQuote(req_columns), collapse = ", "))
  }
}

check_and_fix_MDP <- function(x) {
  x$model <- within(x$model, {
    if (is.numeric(states) &&
        length(states) == 1)
      states <- seq_len(states)
    states <- as.character(states)
    
    if (is.numeric(actions) &&
        length(actions) == 1)
      actions <- seq_len(actions)
    actions <- as.character(actions)
    
    if (inherits(x, "POMDP")) {
      if (is.numeric(observations) &&
          length(observations) == 1)
        observations <- seq_len(observations)
      observations <- as.character(observations)
    }
    
    discount <- as.numeric(discount)
    if (length(discount) != 1 || discount < 0 || discount > 1)
      stop("discount has to be a single value in the range [0,1].")
    
    if (!exists("horizon"))
      horizon <- Inf
    horizon <- as.numeric(horizon)
    
    ## FIXME: check terminal_values
    
    # start
    if (is.numeric(start) && length(start) == length(states)) {
      if (sum(start) != 1)
        stop("The start probability vector does not add up to 1.")
      if (is.null(names(start)))
        names(start) <- states
      else
        start <- start[states]
    }
    if (any(is.na(start)))
      stop("start containes undefined start states.")
    if (is.character(start)) {
      if (!(identical(start, "uniform") || all(start %in% states)))
        stop(
          "when using characters for start, then it needs to be the keyword 'uniform' or a set of start states."
        )
    }
    
    ## read_POMDP does not parse these!
    ## For now, we expand functions into matrices
    if (!exists("problem")) {
      #check_func(transition_prob, T_, "transition_prob")
      if(is.function(transition_prob))
        transition_prob <- transition_matrix(x) 
      #check_func(reward, R_, "reward")
      if(is.function(reward))
        reward <- reward_matrix(x) 
      if (inherits(x, "POMDP"))
        #check_func(observation_prob, O_, "observation_prob")
        if(is.function(observation_prob))
          observation_prob <- observation_matrix(x) 
      
      check_df(transition_prob, T_, "transition_prob")
      check_df(reward, R_, "reward")
      if (inherits(x, "POMDP"))
        check_df(observation_prob, O_, "observation_prob")
      
      ## FIXME: check that a is actions!
      
      # if we have matrices then check and add names
      for (a in names(transition_prob)) {
        if (is.matrix(transition_prob[[a]])) {
          if (!identical(dim(transition_prob[[a]]), c(length(states), length(states))))
            stop("transition_prob matrix for action ",
              a,
              ": has not the right dimensions!")
          if (!all(rowSums(transition_prob[[a]]) == 1))
            stop("transition_prob matrix for action ",
              a,
              ": rows do not add up to 1!")
          dimnames(transition_prob[[a]]) <- list(states, states)
        }
      }
      
      
      for (a in names(reward)) {
        for (s in names(reward[[a]])) {
          if (is.matrix(reward[[a]][[s]])) {
            if (!identical(dim(reward[[a]][[s]]), c(length(states), length(observations))))
              stop(
                "reward matrix for action ",
                a,
                " and start.state ",
                s,
                ": has not the right dimensions!"
              )
            dimnames(reward[[a]][[s]]) <-
              list(states, observations)
          }
        }
      }
      
      if (inherits(x, "POMDP")) {
        for (a in names(observation_prob)) {
          if (is.matrix(observation_prob[[a]])) {
            if (!identical(dim(observation_prob[[a]]), c(length(states), length(observations))))
              stop("observation_prob matrix for action ",
                a,
                ": has not the right dimensions!")
            if (!all(rowSums(observation_prob[[a]]) == 1))
              stop("observation_prob matrix for action ",
                a,
                ": rows do not add up to 1!")
            dimnames(observation_prob[[a]]) <-
              list(states, observations)
          }
        }
      }
    }
  })
  
  x
}




#' @export
print.POMDP <- function(x, ...) {
  cat(paste(class(x), collapse = ", "),
    "-",
    x$model$name,
    "\n")
  
  cat("  Horizon:",
    paste(x$model$horizon, collapse = "+"),
    "\n")
  
  if (!is.null(x$model$horizon) &&
      length(x$model$horizon) > 1)
    cat("time-dependent:",
      length(x$model$horizon),
      "episodes",
      "\n")
  
  if (!is.null(x$solution))
    cat(
      "  Solution converged:",
      x$solution$converged,
      "\n",
      " Total expected reward (for start probabilities):",
      x$solution$total_expected_reward,
      "\n"
    )
  
  cat("  List components:", paste(sQuote(names(x)), collapse = ", "),
    "\n")
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
