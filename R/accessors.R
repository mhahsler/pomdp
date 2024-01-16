#' Access to Parts of the Model Description
#'
#' Functions to provide uniform access to different parts of the POMDP/MDP 
#' problem description.
#'
#' Several parts of the POMDP/MDP description can be defined in different ways. In particular,
#' the fields `transition_prob`, `observation_prob`, `reward`, and `start` can be defined using matrices, data frames or
#' keywords. See [POMDP] for details. The functions provided here, provide unified access to the data in these fields
#' to make writing code easier.
#' 
#' ## Transition Probabilities \eqn{T(s'|s,a)}
#' `transition_matrix()` returns a list with one element for each action. Each element contains a states x states matrix
#' with \eqn{s} (`start.state`) as rows and \eqn{s'} (`end.state`) as columns. 
#' Matrices with a density below 50% can be requested in sparse format (as a [Matrix::dgCMatrix-class])
#' 
#' `transition_val()` retrieves a single entry more efficiently. 
#' 
#' ## Observation Probabilities \eqn{O(o|s',a)}
#' `observation_matrix()` returns a list with one element for each action. Each element contains a states x states matrix
#' with \eqn{s} (`start.state`) as rows and \eqn{s'} (`end.state`) as columns. 
#' Matrices with a density below 50% can be requested in sparse format (as a [Matrix::dgCMatrix-class])
#' 
#' `observation_val()` retrieves a single entry more efficiently. 
#' 
#' ## Reward \eqn{R(s,s',o,a)}
#' `reward_matrix()` returns for the dense representation a list of lists. The list levels are \eqn{a} (`action`)  and \eqn{s} (`start.state`). 
#' The list elements are matrices with rows representing the end state \eqn{s'}  and columns representing observations \eqn{o}. 
#' Many reward structures cannot be efficiently stored using a standard sparse matrix since there might be a fixed cost for each action
#' resulting in no entries with 0. Therefore, the data.frame representation is used as a 'sparse' representation.
#'
#' `observation_val()` retrieves a single entry more efficiently. 
#' 
#' ## Initial Belief 
#' `start_vector()` translates the initial probability vector description into a numeric vector.
#' 
#' ## Convert the Complete POMDP Description into a Consistent Form 
#' `normalize_POMDP()` returns a new POMDP definition where `transition_prob`,
#'    `observations_prob`, `reward`, and `start` are normalized to (lists of) matrices and vectors to
#'    make direct access easy.  Also, `states`, `actions`, and `observations` are ordered as given in the problem
#'    definition to make safe access using numerical indices possible. Normalized POMDP descriptions are used for
#'    C++ based code (e.g., [simulate_POMDP()]) and normalizing them once will save time if the code is
#'    called repeatedly.
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
#' @param drop logical; drop the action list if a single action is requested?
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
#' reward_val(Tiger, action = "open-right", start.state = "tiger-left", end.state = "tiger-left",
#'   observation = "tiger-left")
#'   
#' # Note that the reward in the tiger problem only depends on the action and the start.state 
#' # so we can use:
#' reward_val(Tiger, action = "open-right", start.state = "tiger-left")
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
    sparse = TRUE,
    drop = TRUE) {
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
      sparse = sparse,
      drop = drop
    )
  }

## TODO: make the access functions more efficient for a single value
## NOTE: missing propagation does not work for sparse Matrix...

#' @rdname accessors
#' @export
transition_val <-
  function(x,
    action,
    start.state,
    end.state,
    episode = NULL,
    epoch = NULL) {
    transition_matrix(
      x,
      action = action,
      episode = episode,
      epoch = epoch,
      sparse = NULL
    )[start.state, end.state]
  }

#' @rdname accessors
#' @export
observation_matrix <-
  function(x,
    action = NULL,
    episode = NULL,
    epoch = NULL,
    sparse = TRUE,
    drop = TRUE) {
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
      sparse = sparse,
      drop = drop
    )
  }

#' @rdname accessors
#' @export
observation_val <-
  function(x,
    action,
    end.state,
    observation,
    episode = NULL,
    epoch = NULL) {
    observation_matrix(
      x,
      action = action,
      episode = episode,
      epoch = epoch,
      sparse = NULL
    )[end.state, observation]
  }

#' @rdname accessors
#' @export
reward_matrix <-
  function(x,
    action = NULL,
    start.state = NULL,
    episode = NULL,
    epoch = NULL,
    sparse = FALSE,
    drop = TRUE) {
    ## action list of s' x o matrices
    ## action list of s list of s' x o matrices
    ## if not observations are available then it is a s' vector
    if (is.null(episode)) {
      if (is.null(epoch))
        episode <- 1L
      else
        episode <- epoch_to_episode(x, epoch)
    }
    
    ### sparse = NULL will keep data.frame
    if (.is_timedependent_field(x, "reward"))
      reward <- x[["reward"]][[episode]]
    else
      reward <-  x[["reward"]]
    
    if (is.data.frame(reward) && (is.null(sparse) || sparse))
      return(reward)
    
    mat <- .translate_reward(
      x,
      episode = episode,
      action = action,
      start.state = start.state,
      sparse = sparse
    )
    
    ### unpack if we have a single action/start state
    if (drop && length(mat) == 1L)
      mat <- mat[[1]]
    if (drop && length(mat) == 1L)
      mat <- mat[[1]]
    
    mat
  }

#' @rdname accessors
#' @export
reward_val <-
  function(x,
    action,
    start.state,
    end.state = NA,
    observation = NA,
    episode = NULL,
    epoch = NULL) {
    # we convert NAs to 1L so the subsetting below works!
    
    # end.state can be NA of the reward does not depend on it. 
    if (is.na(end.state)) {
      if (is.data.frame(x$reward))
        if(!all(is.na(x$reward[["end.state"]])))
          stop("The rewards depend on the end.state! Specify the end.state.")
      end.state <- 1L 
    }
    
    # observations are always NA for MDPs
    if (inherits(x, "MDP")) {
      if (!is.na(observation))
        stop("observation has to be 'NA' for MDPs!")
    }
    
    if (is.na(observation)) {
      ### TODO: Checking for matrix format is harder
      if (is.data.frame(x$reward))
        if(!all(is.na(x$reward[["observation"]])))
          stop("The rewards depend on the observation! Specify the observation.")
      observation <- 1L
    }
    
    if (is.numeric(action))
      action <- x$actions[action]
    if (is.numeric(start.state))
      start.state <- x$states[start.state]
    if (is.numeric(end.state))
      end.state <- x$states[end.state]
    if (inherits(x, "POMDP") && is.numeric(observation))
      observation <- x$observations[observation]
    
    if (is.null(episode)) {
      if (is.null(epoch))
        episode <- 1L
      else
        episode <- epoch_to_episode(x, epoch)
    }
    
    if (.is_timedependent_field(x, "reward"))
      rew <- x[["reward"]][[episode]]
    else
      rew <-  x[["reward"]]
    
    ### direct access for data.frame
    if (is.data.frame(rew)) {
      # TODO: need episode interface for cpp
      #rew <- reward_val_from_df_cpp(x, action, start.state, end.state, observation, episode, epoch);  
      #return(rew)
      
      rew <- rew[(rew$action == action | is.na(rew$action)) &
          (rew$start.state == start.state |
              is.na(rew$start.state)) &
          (rew$end.state == end.state | is.na(rew$end.state)) &
          (rew$observation == observation | is.na(rew$observation))
        , , drop = FALSE]
      
      if (nrow(rew) == 0L)
        return(0)
      else
        return(rew$value[nrow(rew)])
    }
    
    reward_matrix(
      x,
      action = action,
      start.state = start.state,
      episode = episode,
      epoch = epoch,
      sparse = NULL
    )[end.state, observation]
  }

#' @rdname accessors
#' @export
start_vector <- function(x) {
  .translate_belief(x$start, model = x)
}

#' @rdname accessors
#' @export
normalize_POMDP <- function(x, sparse = TRUE) {
  if (!is.null(x$normalized) && x$normalized)
    return(x)
    
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
  
  x$normalized <- TRUE
  
  x
}

### TODO: MDP has not time-dependent implementation

#' @rdname accessors
#' @export
normalize_MDP <- function(x, sparse = TRUE) {
  x$start <- start_vector(x)
  x$transition_prob <-
    transition_matrix(x, sparse = sparse)
  x$reward <- reward_matrix(x, sparse = sparse)
  x
}

# make a matrix sparse if it is of low density
.sparsify <- function(x,
  sparse = TRUE,
  max_density = .5) {
  # NULL means as is, we also keep special keywords
  if (is.null(sparse) || is.character(x))
    return(x)
  
  if (!sparse)
    return(as.matrix(x))
 
  ### make sparse 
  if (nnzero(x) / length(x) < max_density)
    return(as(as(x, "generalMatrix"), "CsparseMatrix"))
  else
    return(as.matrix(x))
}

# build a sparse triplet matrix from POMDP data.table (deals with NAs)
.sparse_from_vectors <- function(i, j, x, from, to) {
  ### expand NAs
  i_NA <- is.na(i)
  j_NA <- is.na(j)
  
  if (any(i_NA) || any(j_NA)) {
    both_NA <- sum(i_NA & j_NA)
    i_NA <- sum(i_NA) - both_NA
    j_NA <- sum(j_NA) - both_NA
    
    nlen <- length(i) + i_NA * (length(to) - 1L) +
      j_NA * (length(from) - 1L) +
      both_NA * (length(to) + length(from) - 1L)
    
    i_n <- integer(nlen)
    j_n <- integer(nlen)
    x_n <- numeric(nlen)
    
    ii_n <- 1L
    for (ii in seq_along(i)) {
      ## this case makes it into a dense matrix!!!
      if (is.na(i[ii]) && is.na(j[ii])) {
        for (kk in seq_along(from)) {
          for (kkk in seq_along(to)) {
            i_n[ii_n] <- kk
            j_n[ii_n] <- kkk
            x_n[ii_n] <- x[ii]
            ii_n <- ii_n + 1L
          }
        }
      } else if (is.na(i[ii])) {
        for (kk in seq_along(from)) {
          i_n[ii_n] <- kk
          j_n[ii_n] <- j[ii]
          x_n[ii_n] <- x[ii]
          ii_n <- ii_n + 1L
        }
      } else if (is.na(j[ii])) {
        for (kk in seq_along(to)) {
          i_n[ii_n] <- i[ii]
          j_n[ii_n] <- kk
          x_n[ii_n] <- x[ii]
          ii_n <- ii_n + 1L
        }
      } else {
        i_n[ii_n] <- i[ii]
        j_n[ii_n] <- j[ii]
        x_n[ii_n] <- x[ii]
        ii_n <- ii_n + 1L
      }
      
      
      
      if (is.na(i[ii])) {
        
      } else {
        
      }
    }
    
    i <- i_n
    j <- j_n
    x <- x_n
  }
  
  dd <- duplicated(cbind(i, j), fromLast = TRUE)
  
  if (any(dd)) {
    i <- i[!dd]
    j <- j[!dd]
    x <- x[!dd]
  }
  
  m <-
    methods::new(
      "dgTMatrix",
      i = rev(as.integer(i) - 1L),
      j = rev(as.integer(j) - 1L),
      x = rev(as.numeric(x)),
      Dim = c(length(from), length(to))
    )
  dimnames(m) <- list(from, to)
  m
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
    
    m <- .sparse_from_vectors(
      i = as.integer(df[, 1]),
      j = as.integer(df[, 2]),
      x = as.numeric(df[, 3]),
      from = as.character(model[[from]]),
      to = as.character(model[[to]])
    )
    
    .sparsify(m, sparse = sparse)
  }

### translate ids to names (factor)
.get_names <- function(x, names) {
  x[x == "*"] <- NA
  x <- type.convert(x, as.is = TRUE)
  
  if (!is.numeric(x))
    y <- factor(x, levels = names)
  else
    y <- factor(x, levels = seq_along(names), labels = names)
  
  if (any(is.na(y) & !is.na(x)))
    stop("Unknown action, state or observation label(s): ", paste(x[is.na(y) & !is.na(x)], collapse = ", "))
  
  y
}

# converts any description into a matrix and fixes it up
.translate_probabilities <- function(model,
  field = "transition_prob",
  from = "states",
  to = "states",
  episode = 1,
  action = NULL,
  sparse = TRUE,
  drop = TRUE) {
  if (is.null(action))
    actions <- model$actions
  else
    actions <- as.character(.get_names(action, model$actions))
  
  if (any(is.na(actions)))
    stop("Unknown action!")
  
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
      names(prob) <- model$actions
      
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
  
  if (drop && !is.null(action) && length(action) == 1)
    prob <- prob[[1]]
  
  prob
}


## reward is action -> start.state -> end.state x observation
## drop is done in the calling function
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
      action <- model$actions
    actions <- as.character(.get_names(action, model$actions))
    
    if (is.null(start.state))
      start.state <- states
    start.states <- as.character(.get_names(start.state, states))
    
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
    
    else if (is.data.frame(reward)) {
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
              start.states,
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
            if (!(acts %in% actions))
              next
          
          ss_from <- reward[i, 2L]
          if (is.na(ss_from))
            ss_from <- states
          ss_from <- intersect(ss_from, start.states)
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
              start.states,
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
          ss_from <- intersect(ss_from, start.states)
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
            start.states,
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
    } else {
    ### FIXME: missing. Also version without observations!
    ## translate from list of matrices (fix names, and deal with "identity", et al)
    ##} else if (is.list(reward)) {
    ##}
    ## should be a list of list of matrices
      if(is.null(action) && is.null(start.state))
        reward <- model$reward
      else 
        reward <- sapply(actions, FUN = function(a) model$reward[[a]][start.states], simplify = FALSE, USE.NAMES = TRUE)
    }
      
    ### MDPs have vectors not matrices here!
    if (!inherits(model, "MDP")) {
      reward <- lapply(reward, lapply, .sparsify, sparse)
    }
    
    reward
  }