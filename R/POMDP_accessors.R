#' Access to Parts of the POMDP Description
#'
#' Functions to provide uniform access to different parts of the POMDP description.
#'
#' Several parts of the POMDP description can be defined in different ways. In particular,
#' `transition_prob`, `observation_prob`, `reward`, and `start` can be defined using matrices, data frames or
#' keywords. See [POMDP] for details. The functions provided here, provide unified access to the data in these fields
#' to make writing code easier.
#'
#' * `start_vector()` translates the initial probability vector into a vector.
#' * For `transition_prob`, `observation_prob`, `reward`, functions ending in `_matrix()` and `_val()` are
#'    provided to access the data as lists of matrices, a matrix or a scalar value. For `_matrix()`,
#'    matrices with a density below 50% can be requested in sparse format (as a [Matrix::dgCMatrix-class]).
#' * `normalize_POMDP()` returns a new POMDP definition where `transition_prob`,
#'    `observations_prob`, `reward`, and `start` are normalized to (lists of) matrices and vectors to
#'    make direct access easy.  Also, `states`, `actions`, and `observations` are ordered as given in the problem
#'    definition to make safe access using numerical indices possible. Normalized POMDP descriptions are used for
#'    C++ based code (e.g., [simulate_POMDP()]) and normalizing them once will save time if the code is
#'    called repeatedly.
#'
#' @family POMDP
#' @family MDP
#' @name POMDP_accessors
#'
#' @param x A [POMDP] or [MDP] object.
#' @param action name or index of an action.
#' @param start.state,end.state name or index of the state.
#' @param observation name or index of observation.
#' @param episode,epoch Episode or epoch used for time-dependent POMDPs. Epoch are internally converted 
#'  to the episode using the model horizon. 
#' @param sparse logical; use sparse matrices when the density is below 50% . `NULL` returns the
#'   representation stored in the problem description.
#' @return A list or a list of lists of matrices.
#' @author Michael Hahsler
#' @examples
#' data("Tiger")
#'
#' # List of |A| transition matrices. One per action in the from states x states
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
#' reward_val(Tiger, action = "open-right", start.state = "tiger-left", end.state = "tiger-left", 
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
#' library("igraph")
#' g <- graph_from_adjacency_matrix(transition_matrix(Tiger, action = "open-left"), weighted = TRUE)
#' edge_attr(g, "label") <- edge_attr(g, "weight")
#'
#' igraph.options("edge.curved" = TRUE)
#' plot(g, layout = layout_on_grid, main = "Transitions for action 'open=left'")
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
#' @export
transition_matrix <-
  function(x,
    action = NULL,
    episode = NULL,
    epoch = NULL,
    sparse = TRUE) {
    if (is.null(episode)) {
      if (is.null(epoch))
        episode <- 1L
      else
        episode <- epoch_to_episode(x, epoch)
    }
    
    .translate_probabilities(
      x,
      field = "transition_prob",
      from = "states",
      to = "states",
      episode = episode,
      action = action,
      sparse = sparse
    )
  }

## TODO: make the access functions more efficient for a single value
## NOTE: missing propagation does not work for sparse Matrix...

#' @rdname POMDP_accessors
#' @export
transition_val <-
  function(x,
    action,
    start.state,
    end.state,
    episode = NULL,
    epoch = NULL) {
    transition_matrix(x,
      action = action,
      episode = episode,
      epoch = epoch,
      sparse = NULL)[start.state, end.state]
  }

#' @rdname POMDP_accessors
#' @export
observation_matrix <-
  function(x,
    action = NULL,
    episode = NULL,
    epoch = NULL,
    sparse = TRUE) {
    if (is.null(x$observation_prob))
      stop("model is not a complete POMDP, no observation probabilities specified!")
    
    if (is.null(episode)) {
      if (is.null(epoch))
        episode <- 1L
      else
        episode <- epoch_to_episode(x, epoch)
    }
    
    ## action list of s' x o matrices
    .translate_probabilities(
      x,
      field = "observation_prob",
      from = "states",
      to = "observations",
      episode = episode,
      action = action,
      sparse = sparse
    )
  }

#' @rdname POMDP_accessors
#' @export
observation_val <-
  function(x,
    action,
    end.state,
    observation,
    episode = NULL,
    epoch = NULL) {
    observation_matrix(x,
      action = action,
      episode = episode,
      epoch = epoch,
      sparse = NULL)[end.state, observation]
  }

#' @rdname POMDP_accessors
#' @export
reward_matrix <-
  function(x,
    action = NULL,
    start.state = NULL,
    episode = NULL,
    epoch = NULL,
    sparse = FALSE) {
    ## action list of s' x o matrices
    ## action list of s list of s' x o matrices
    ## if not observations are available then it is a s' vector
    if (is.null(episode)) {
      if (is.null(epoch))
        episode <- 1L
      else
        episode <- epoch_to_episode(x, epoch)
    }
    
    mat <- .translate_reward(
      x,
      episode = episode,
      action = action,
      start.state = start.state,
      sparse = sparse
    )
    
    ### unpack if we have a single action/start state
    if (length(mat) == 1L)
      mat <- mat[[1]]
    if (length(mat) == 1L)
      mat <- mat[[1]]
      
   mat 
  }

#' @rdname POMDP_accessors
#' @export
reward_val <-
  function(x,
    action,
    start.state,
    end.state,
    observation,
    episode = NULL,
    epoch = NULL) {
    reward_matrix(
      x,
      action = action,
      start.state = start.state,
      episode = episode,
      epoch = epoch,
      sparse = NULL
    )[end.state, observation]
  }

#' @rdname POMDP_accessors
#' @export
start_vector <- function(x) {
  .translate_belief(x$start, model = x)
}

#' @rdname POMDP_accessors
#' @export
normalize_POMDP <- function(x, sparse = TRUE) {
  x$start <- start_vector(x)
  
  if (.is_timedependent_field(x, "transition_prob")) {
    for (i in seq_along(x$transition_prob))
      x$transition_prob[[i]] <-
        transition_matrix(x, episode = i, sparse = sparse)
  } else
    x$transition_prob <-
      transition_matrix(x, sparse = sparse)
  
  if (.is_timedependent_field(x, "observation_prob")) {
    for (i in seq_along(x$observation_prob))
      x$observation_prob[[i]] <-
        observation_matrix(x, episode = i, sparse = sparse)
  } else
    x$observation_prob <-
      observation_matrix(x, sparse = sparse)
  
  if (.is_timedependent_field(x, "reward")) {
    for (i in seq_along(x$reward))
      x$reward[[i]] <-
        reward_matrix(x, episode = i, sparse = sparse)
  } else
    x$reward <-
      reward_matrix(x, sparse = sparse)
  
  x
}

### TODO: MDP has not time-dependent implementation

#' @rdname POMDP_accessors
#' @export
normalize_MDP <- function(x, sparse = TRUE) {
  
  x$start <- start_vector(x)
  x$transition_prob <-
    transition_matrix(x, sparse = sparse)
  x$reward <- reward_matrix(x, sparse = sparse)
  x
}

# translate different specifications of transitions, observations and rewards
# into a list of matrices

.sparsify <- function(x,
  sparse = TRUE,
  max_density = .5) {
  # NULL mans as is
  if (is.null(sparse))
    return(x)
  
  if (!sparse)
    return(as.matrix(x))
  
  if (nnzero(x) / length(x) < max_density)
    return(as(x , "CsparseMatrix"))
  else
    return(as.matrix(x))
}

# df needs to have 3 columns: from, to, and val
.df2matrix <-
  function(model,
    df,
    from = "states",
    to = "observations",
    sparse = TRUE) {
    # default is sparse
    if (is.null(sparse))
      sparse <- TRUE
    
    from <- as.character(model[[from]])
    to <- as.character(model[[to]])
    
    ### make sure we have character in from/to (pre R 4.0)
    
    df[, 1] <- .get_names(df[, 1], from)
    df[, 2] <- .get_names(df[, 2], to)
    
    # build sparse matrix using tiplet format
    if (sparse) {
      m <- spMatrix(nrow = length(from), ncol = length(to))
      dimnames(m) <- list(from, to)
    } else {
      m <- matrix(
        0,
        nrow = length(from),
        ncol = length(to),
        dimnames = list(from, to)
      )
    }
    
    for (i in 1:nrow(df)) {
      if (is.na(df[i, 1]) && is.na(df[i, 2]))
        m[] <- df[i, 3]
      else if (is.na(df[i, 1]))
        m[, df[i, 2]] <- df[i, 3]
      else if (is.na(df[i, 2]))
        m[df[i, 1],] <- df[i, 3]
      else
        m[df[i, 1], df[i, 2]] <- df[i, 3]
    }
    
    # this will make the triplet matrix into a dgCMatrix or a dense matrix
    .sparsify(m, sparse)
  }

# df needs to have 2 columns: to and val
.df2vector <-
  function(model, df, from = "states") {
    from <- as.character(model[[from]])
    
    df[, 1] <- .get_names(df[, 1], from)
    
    v <- numeric(length(from))
    names(v) <- from
    
    for (i in 1:nrow(df)) {
      if (is.na(df[i, 1]))
        v[] <- df[i, 2]
      else
        v[df[i, 1]] <- df[i, 2]
    }
    
    v
  }


### translate ids to names
.get_names <- function(x, names) {
  x[x == "*"] <- NA
  x <- type.convert(x, as.is = TRUE)
  
  if (!is.numeric(x))
    as.character(x)
  else
    as.character(factor(x, levels = seq_along(names), labels = names))
}

.translate_probabilities <- function(model,
  field = "transition_prob",
  from = "states",
  to = "states",
  episode = 1,
  action = NULL,
  sparse = TRUE) {
  if (is.null(action))
    actions <- model$actions
  else
    actions <- action
  
  ## episodes are for time-dependent POMDPs
  if (is_timedependent_POMDP(model)) {
    episode <- as.integer(episode)
    if (episode < 1L ||
        episode > length(model$horizon))
      stop("Requested episode does not exit in horizon specification.")
    
    if (.is_timedependent_field(model, field)) {
      prob <- model[[field]][[episode]]
      if (is.null(prob))
        stop(
          "Inconsistent POMDP definition. Requested episode does not exit in field ",
          field,
          "."
        )
    } else
      prob <-  model[[field]]
  } else
    prob <-  model[[field]]
  
  if (is.null(prob))
    stop(
      "Field ",
      field,
      " is not available. ",
      "Note: Parsing some fields may be disabled with read_POMDP!"
    )
  
  
  ## translate from dataframes
  if (is.data.frame(prob)) {
    prob <- sapply(actions, function(a) {
      .df2matrix(model,
        prob[(prob$action == a | is.na(prob$action)), 2:4],
        from = from,
        to = to,
        sparse = sparse)
    }, simplify = FALSE, USE.NAMES = TRUE)
    
    ## translate from function
  } else if (is.function(prob)) {
    prob <- Vectorize(prob)
    prob <- sapply(actions, function(a) {
      p <- outer(
        model[[from]],
        model[[to]],
        FUN = function(from, to)
          prob(a,
            from,
            to)
      )
      dimnames(p) <- list(model[[from]], model[[to]])
      .sparsify(p, sparse)
    }, simplify = FALSE)
    
    
    ## translate from list of matrices (fix order names, and deal with "identity", et al)
  } else if (is.list(prob)) {
    ## fix order in list
    if (is.null(names(prob)))
      names(prob) <- actions
    else
      prob <- prob[actions]
    
    ## translate to matrix and fix order or rows and columns
    from <- as.character(model[[from]])
    to <- as.character(model[[to]])
    
    prob <- lapply(
      prob,
      FUN = function(tr) {
        if (is.character(tr)) {
          tr <- switch(
            tr,
            identity = {
              if (is.null(sparse) || sparse) 
                Matrix::Diagonal(length(from))
              else
                diag(length(from))
            },
            uniform = matrix(
              1 / length(to),
              nrow = length(from),
              ncol = length(to)
            )
          )
          
          dimnames(tr) <- list(from, to)
          tr
        }
        
        if (!is.matrix(tr) && !inherits(tr, "Matrix"))
          stop("Probabilities cannot be converted to matrix.")
        
        ## fix names and order
        if (is.null(dimnames(tr)))
          dimnames(tr) <- list(from, to)
        else
          tr <- tr[from, to]
        
        .sparsify(tr, sparse)
      }
    )
  } else
    stop("Unknown ", field, " matrix format.\n")
  
  if (!is.null(action) && length(action) == 1)
    prob <- prob[[1]]
  
  prob
}


## reward is action -> start.state -> end.state x observation
.translate_reward <-
  function(model,
    episode = 1,
    action = NULL,
    start.state = NULL,
    sparse = FALSE) {
    field <- "reward"
    
    states <- model$states
    observations <- model$observations
    if (is.null(action))
      actions <- model$actions
    else
      actions <- .get_names(action, model$actions)
    
    if (is.null(start.state))
      start.state <- states
    else
      start.state <- .get_names(start.state, states)
    
    ## episodes are for time-dependent POMDPs
    if (.is_timedependent_field(model, field))
      reward <- model[[field]][[episode]]
    else
      reward <-  model[[field]]
      
    if (is.null(reward)) {
      stop(
        "Field ",
        field,
        " is not available. Parsing some fields is not implemented for models read with read_POMDP!"
      )
      return (NULL)
    }
      
    if (is.data.frame(reward)) {
      reward[[1L]] <- .get_names(reward[[1L]], model$actions)
      reward[[2L]] <- .get_names(reward[[2L]], states) # start state
      reward[[3L]] <- .get_names(reward[[3L]], states) # end state
      reward[[4L]] <- .get_names(reward[[4L]], observations)
      
      # allocate the memory
      if (inherits(model, "POMDP")) {
        mat <- sapply(
          actions,
          FUN = function(a)
            sapply(
              start.state,
              FUN = function(s)
                if (!is.null(sparse) && sparse) {
                  m <- spMatrix(nrow = length(states),
                    ncol = length(observations))
                  dimnames(m) <- list(states, observations)
                  m
                } else {
                  matrix(
                    0,
                    nrow = length(states),
                    ncol = length(observations),
                    dimnames = list(states, observations)
                  )
                },
              simplify = FALSE
            ),
          simplify = FALSE
        )
        
        for (i in seq_len(nrow(reward))) {
          acts <- reward[i, 1L]
          if (is.na(acts))
            acts <- actions
          else
            if (is.null(mat[[acts]]))
              next
          ss_from <- reward[i, 2L]
          if (is.na(ss_from))
            ss_from <- states
          ss_from <- intersect(ss_from, start.state)
          if (length(ss_from) == 0L)
            next
          
          ss_to <- reward[i , 3L]
          if (is.na(ss_to))
            ss_to <- states
          os <- reward[i, 4L]
          if (is.na(os))
            os <- observations
          val <- reward[i, 5L]
          
          for (a in acts)
            for (s_from in ss_from)
              mat[[a]][[s_from]][ss_to, os] <- val
        }
      } else {
        ### MDP has vectors and the value is in position 4
        mat <- sapply(
          actions,
          FUN = function(a)
            sapply(
              start.state,
              FUN = function(s) {
                v <- numeric(length(states))
                names(v) <- states
                v
              },
              simplify = FALSE
            ),
          simplify = FALSE
        )
        
        for (i in seq_len(nrow(reward))) {
          acts <- reward[i, 1L]
          if (is.na(acts))
            acts <- actions
          ss_from <- reward[i, 2L]
          if (is.na(ss_from))
            ss_from <- states
          ss_from <- intersect(ss_from, start.state)
          if (length(ss_from) == 0L)
            next
          
          ss_to <- reward[i, 3L]
          if (is.na(ss_to))
            ss_to <- states
          val <- reward[i, 5L]
          
          for (a in acts)
            for (s_from in ss_from)
              mat[[a]][[s_from]][ss_to] <- val
        }
      }
      
      reward <- mat
      
      ## translate from function
    } else if (is.function(reward)) {
      reward <- Vectorize(reward)
      reward <- sapply(
        actions,
        FUN = function(a)
          sapply(
            start.state,
            FUN = function(s) {
              p <- outer(
                states,
                observations,
                FUN = function(end, o)
                  reward(
                    action = a,
                    start.state = s,
                    end.state = end,
                    observation = o
                  )
              )
              dimnames(p) <- list(states, observations)
              p <- .sparsify(p, sparse)
            },
            simplify = FALSE
          ),
        simplify = FALSE
      )
    }
    
    ### FIXME: missing. Also version without observations!
    ## translate from list of matrices (fix names, and deal with "identity", et al)
    ##} else if (is.list(reward)) {
    ##}
    
    ### MDPs have vectors not matrices here!
    if (!inherits(model, "MDP")) {
        reward <- lapply(reward, lapply, .sparsify, sparse)
    }
    
    reward
  }