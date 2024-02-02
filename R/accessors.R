#' Access to Parts of the Model Description
#'
#' Functions to provide uniform access to different parts of the POMDP/MDP
#' problem description.
#'
#' Several parts of the POMDP/MDP description can be defined in different ways. In particular,
#' the fields `transition_prob`, `observation_prob`, `reward`, and `start` can be defined using matrices, data frames,
#' keywords, or functions. See [POMDP] for details. The functions provided here, provide unified access to the data in these fields
#' to make writing code easier.
#'
#' ## Transition Probabilities \eqn{T(s'|s,a)}
#' `transition_matrix()` accesses the transition model. The complete model
#' is a list with one element for each action. Each element contains a states x states matrix
#' with \eqn{s} (`start.state`) as rows and \eqn{s'} (`end.state`) as columns.
#' Matrices with a density below 50% can be requested in sparse format 
#' (as a [Matrix::dgCMatrix-class]).
#'
#' ## Observation Probabilities \eqn{O(o|s',a)}
#' `observation_matrix()` accesses the observation model. The complete model is a 
#' list with one element for each action. Each element contains a states x observations matrix
#' with \eqn{s} (`start.state`) as rows and \eqn{o} (`observation`) as columns.
#' Matrices with a density below 50% can be requested in sparse format 
#' (as a [Matrix::dgCMatrix-class])
#'
#' ## Reward \eqn{R(s,s',o,a)}
#' `reward_matrix()` accesses the reward model. 
#' The preferred representation is a data.frame with the 
#' columns `action`, `start.state`, `end.state`, 
#' `observation`, and `value`. This is a sparse representation. 
#' The dense representation is a list of lists of matrices. 
#' The list levels are \eqn{a} (`action`)  and \eqn{s} (`start.state`).
#' The matrices have rows representing \eqn{s'} (`end.state`) 
#' and columns representing \eqn{o} (`observations`).
#' The reward structure cannot be efficiently stored using a standard sparse matrix 
#' since there might be a fixed cost for each action
#' resulting in no entries with 0.
#'
#' ## Initial Belief
#' `start_vector()` translates the initial probability vector description into a numeric vector.
#'
#' ## Convert the Complete POMDP Description into a consistent form
#' `normalize_POMDP()` returns a new POMDP definition where `transition_prob`,
#' `observations_prob`, `reward`, and `start` are normalized. The options are:
#' 
#' * `sparse = NULL`: Only translates functions and leaves everything else as in the description. This avoids 
#'    unnecessary conversions.
#' * `sparse = TRUE`: Converts everything into dense matrices. This allows fast access for small problems,
#'    but requires too much memory for larger problems. 
#' * `sparse = FALSE`: Transitions and observations are stored as sparse [Matrix::dgCMatrix-class] objects.
#'    Rewards are stored as a data.frame.
#' 
#' 
#'  
#' 
#' to (lists of) matrices and vectors to
#' make direct access easy.  Also, `states`, `actions`, and `observations` are ordered as given in the problem
#' definition to make safe access using numerical indices possible. Normalized POMDP descriptions are used for
#' C++ based code (e.g., [simulate_POMDP()]) and normalizing them once will save time if the code is
#' called repeatedly.
#'
#' @family POMDP
#' @family MDP
#' @name accessors
#'
#' @param x A [POMDP] or [MDP] object.
#' @param action name or index of an action.
#' @param start.state,end.state name or index of the state.
#' @param observation name or index of observation.
#' @param episode,epoch Episode or epoch used for time-dependent POMDPs. Epochs are internally converted
#'  to the episode using the model horizon.
#' @param sparse logical; use sparse matrices when the density is below 50% and keeps data.frame representation
#'  for the reward field. `NULL` returns the
#'   representation stored in the problem description which saves the time for conversion.
#' @return A list or a list of lists of matrices.
#' @author Michael Hahsler
#' @examples
#' data("Tiger")
#'
#' # List of |A| transition matrices. One per action in the from start.states x end.states
#' Tiger$transition_prob
#' transition_matrix(Tiger)
#' transition_val(Tiger, action = "listen", start.state = "tiger-left", end.state = "tiger-left")
#'
#' # List of |A| observation matrices. One per action in the from states x observations
#' Tiger$observation_prob
#' observation_matrix(Tiger)
#' observation_val(Tiger, action = "listen", end.state = "tiger-left", observation = "tiger-left")
#'
#' # List of list of reward matrices. 1st level is action and second level is the
#' #  start state in the form end state x observation
#' Tiger$reward
#' reward_matrix(Tiger)
#' reward_matrix(Tiger, sparse = TRUE)
#' reward_matrix(Tiger, action = "open-right", start.state = "tiger-left", end.state = "tiger-left",
#'   observation = "tiger-left")
#'
#' # Translate the initial belief vector
#' Tiger$start
#' start_vector(Tiger)
#'
#' # Normalize the whole model
#' Tiger_norm <- normalize_POMDP(Tiger)
#' Tiger_norm$transition_prob
#'
#' ## Visualize transition matrix for action 'open-left'
#' plot_transition_graph(Tiger)
#' 
#' ## Use a function for the Tiger transition model
#' trans <- function(action, end.state, start.state) {
#'   ## listen has an identity matrix
#'   if (action == 'listen')
#'     if (end.state == start.state) return(1)
#'     else return(0)
#'
#'   # other actions have a uniform distribution
#'   return(1/2)
#' }
#'
#' Tiger$transition_prob <- trans
#'
#' # transition_matrix evaluates the function
#' transition_matrix(Tiger)
NULL

#' @rdname accessors
#' @export
start_vector <- function(x) {
  .translate_belief(x$start, model = x)
}

#' @rdname accessors
#' @export
normalize_POMDP <- function(x, sparse = TRUE) {
  if (!inherits(x, "POMDP"))
    stop("x is not an POMDP object!")
  
  x$start <- start_vector(x)
  
  # transitions to matrices
  if (.is_timedependent_field(x, "transition_prob")) {
    for (i in seq_along(x$transition_prob))
      x$transition_prob[[i]] <-
        transition_matrix(x, episode = i, sparse = sparse)
  } else
    x$transition_prob <-
      transition_matrix(x, sparse = sparse)
  
  # observations to matrices
  if (.is_timedependent_field(x, "observation_prob")) {
    for (i in seq_along(x$observation_prob))
      x$observation_prob[[i]] <-
        observation_matrix(x, episode = i, sparse = sparse)
  } else
    x$observation_prob <-
      observation_matrix(x, sparse = sparse)
  
  # reward to data.frame (sparse) or dense matrix
  if (.is_timedependent_field(x, "reward")) {
    for (i in seq_along(x$reward))
      x$reward[[i]] <-
        reward_matrix(x, episode = i, sparse = sparse)
  } else {
    x$reward <-
      reward_matrix(x, sparse = sparse)
  }
  
  x
}

### TODO: MDP has no time-dependent implementation

#' @rdname accessors
#' @export
normalize_MDP <- function(x, sparse = TRUE) {
  if (!inherits(x, "MDP"))
    stop("x is not an MDP object!")
  x$start <- start_vector(x)
  
  x$transition_prob <-
    transition_matrix(x, sparse = sparse)
  
  x$reward <- reward_matrix(x, sparse = sparse)
  x
}
    

# make a matrix sparse if it has low density
.sparsify <- function(x,
                      sparse = TRUE,
                      max_density = .5) {
  # NULL means as is, we also keep special keywords
  if (is.null(sparse) || is.character(x))
    return(x)
  
  if (!sparse) {
    if (is.matrix(x)) return(x)
    else return(as.matrix(x))
  }
  
  # sparse
  if (inherits(x, "CsparseMatrix"))
    return(x)
  
  if (nnzero(x) / length(x) < max_density)
    return(as(as(x, "generalMatrix"), "CsparseMatrix"))
  else
    return(as.matrix(x))
}

