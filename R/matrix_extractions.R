#' Extract the Transition, Observation or Reward Information from a POMDP
#'
#' Converts the description of transition probabilities and observation
#' probabilities in a POMDP into a list of matrices. Individual values or parts of the matrices
#' can be more efficiently retrieved using the functions ending `_prob` and `_val`. 
#'
#' `normalize_POMDP()` returns a new POMDP definition where `transition_prob`, `observations_prob`,
#' `reward`, and `start` are normalized to (lists of) matrices and vectors to make access easy. 
#' Also, `states`, `actions`, and `observations` are ordered as given in the problem definition to make safe
#' access using numerical indices possible.
#' 
#' `start_vector` normalizes the initial belief vector.
#' 
#' See Details section in [POMDP] for more details about possible formats for `transition_prob`, `observations_prob`,
#' `reward`, and `start`.
#'
#' @family POMDP
#' @name matrix_extractions
#' 
#' @param x A [POMDP] object.
#' @param episode Episode used for time-dependent POMDPs ([POMDP]).
#' @param action only return the matrix/value for a given action.
#' @param start.state,end.state name of the state.
#' @param observation name of observation. 
#' @return A list or a list of lists of matrices.
#' @author Michael Hahsler
#' @examples
#' data("Tiger")
#'
#' # List of |A| transition matrices. One per action in the from states x states
#' Tiger$transition_prob
#' transition_matrix(Tiger)
#' transition_prob(Tiger, action = "listen", start.state = "tiger-left")
#'
#' # List of |A| observation matrices. One per action in the from states x observations
#' Tiger$observation_prob
#' observation_matrix(Tiger)
#' observation_prob(Tiger, action = "listen", end.state = "tiger-left")
#'
#' # List of list of reward matrices. 1st level is action and second level is the
#' #  start state in the form end state x observation
#' Tiger$reward
#' reward_matrix(Tiger)
#' reward_val(Tiger, action = "listen", start.state = "tiger")
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
#' g <- graph_from_adjacency_matrix(transition_matrix(Tiger)$"open-left", weighted = TRUE)
#' edge_attr(g, "label") <- edge_attr(g, "weight")
#'
#' igraph.options("edge.curved" = TRUE)
#' plot(g, layout = layout_on_grid, main = "Transitions for action 'open=left'")
#'
#' ## Use a function for the Tiger transition model
#' trans <- function(action, end.state, start.state) {
#'   ## listen has an identity matrix
#'   if(action == 'listen')
#'     if(end.state == start.state) return(1)
#'     else return(0)
#'
#'   # other actions have a uniform distribution
#'   return(1/2)
#' }
#'
#' Tiger$transition_prob <- trans
#' transition_matrix(Tiger)
#' @export
transition_matrix <- function(x, episode = 1, action = NULL) {
  .translate_probabilities(
    x,
    field = "transition_prob",
    from = "states",
    to = "states",
    episode = episode,
    action = action
  )
} 

## TODO: make the access functions more efficient for a single value

#' @rdname matrix_extractions
#' @export
transition_prob <- function(x, action, start.state, end.state, episode = 1) {
  transition_matrix(x, episode = 1, action = action)[start.state, end.state]
}

#' @rdname matrix_extractions
#' @export
observation_matrix <- function(x, episode = 1, action = NULL) {
  if(is.null(x$observation_prob))
    stop("model is not a complete POMDP, no observation probabilities specified!")
  
  ## action list of s' x o matrices
  .translate_probabilities(
    x,
    field = "observation_prob",
    from = "states",
    to = "observations",
    episode = episode,
    action = action
  )
}

#' @rdname matrix_extractions
#' @export
observation_prob <- function(x, action, end.state, observation, episode = 1) {
  observation_matrix(x, episode = 1, action = action)[end.state, observation]
}
  
#' @rdname matrix_extractions
#' @export
reward_matrix <- function(x, episode = 1, action = NULL, start.state = NULL) {
  ## action list of s' x o matrices
  ## action list of s list of s' x o matrices
  ## if not observations are available then it is a s' vector
  .translate_reward(x, episode = episode, action = action, start.state = start.state)
}

#' @rdname matrix_extractions
#' @export
reward_val <- function(x, action, start.state, end.state, observation, episode = 1) {
  reward_matrix(x, episode = 1, action = action, start.state = start.state)[end.state, observation]
}

#' @rdname matrix_extractions
#' @export
start_vector <- function(x) {
  .translate_belief(x$start, model = x)
}

#' @rdname matrix_extractions
#' @export
normalize_POMDP <- function(x, episode = 1) {
  x$start <- start_vector(x)
  x$transition_prob <- transition_matrix(x, episode)
  x$observation_prob <- observation_matrix(x, episode)
  x$reward <- reward_matrix(x, episode) 
  x
}

#' @rdname matrix_extractions
#' @export
normalize_MDP <- function(x, episode = 1) {
  x$start <- start_vector(x)
  x$transition_prob <- transition_matrix(x, episode)
  x$reward <- reward_matrix(x, episode) 
  x
}

# translate different specifications of transitions, observations and rewards
# into a list of matrices

# df needs to have 3 columns: from, to, and val
.df2matrix <-
  function(model, df, from = "states", to = "observations") {
    from <- as.character(model[[from]])
    to <- as.character(model[[to]])
    
    ### make sure we have character in from/to (pre R 4.0)
    
    df[, 1] <- .get_names(df[, 1], from)
    df[, 2] <- .get_names(df[, 2], to)
    
    m <- matrix(
      0,
      nrow = length(from),
      ncol = length(to),
      dimnames = list(from, to)
    )
    
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
    
    m
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
  if (!is.numeric(x))
    as.character(x)
  else
    as.character(factor(x, labels = names))
}

.translate_probabilities <- function(model,
  field = "transition_prob",
  from = "states",
  to = "states",
  episode = 1,
  action = NULL) {
  
  if (is.null(action))
    actions <- model$actions
  else
    actions <- action
  
  ## episodes are for time-dependent POMDPs
  if (.timedependent_POMDP(model)) {
    episode <- as.integer(episode)
    if (episode < 1L ||
        episode > length(model$horizon))
      stop("Requested episode does not exit in horizon specification.")
    
    if (.is_timedependent(model, field)) {
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
      "Note: Parsing some fields is not implemented for models read with read_POMDP!"
    )
  
  
  ## translate from dataframes
  if (is.data.frame(prob)) {
    prob <- sapply(actions, function(a) {
      .df2matrix(model,
        prob[(prob$action == a | is.na(prob$action)), 2:4],
        from = from, to = to)
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
      p
    }, simplify = FALSE, USE.NAMES = TRUE)
    
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
            identity = diag(1, nrow = length(from), ncol = length(to)),
            uniform = matrix(
              1 / length(to),
              nrow = length(from),
              ncol = length(to)
            )
          )
        }
        
        if (!is.matrix(tr))
          stop("Probabilities cannot be converted to matrix.")
        
        ## fix names and order
        if (is.null(dimnames(tr)))
          dimnames(tr) <- list(from, to)
        else
          tr <- tr[from, to]
        
        tr
      }
    )
  } else
    stop("Unknown transition/observation matrix format.")
  
  if(!is.null(action) && length(action) == 1) 
    prob <- prob[[1]]
  
  prob
}


## reward is action -> start.state -> end.state x observation
.translate_reward <- function(model, episode = 1, action = NULL, start.state = NULL) {
  if (is.null(start.state))
    states <- model$states
  else
    states <- start.state
  
  if (is.null(action))
    actions <- model$actions
  else
    actions <- action
  
  ## note: observations does not exist for MDPs
  observations <- model$observations
  
  field <- "reward"
  ## episodes are for time-dependent POMDPs
  if (.timedependent_POMDP(model)) {
    episode <- as.integer(episode)
    if (episode < 1L ||
        episode > length(model$horizon))
      stop("Requested episode does not exit.")
    
    if (.is_timedependent(model, field))
      reward <- model[[field]][[episode]]
    else
      reward <-  model[[field]]
    
  } else {
    if (episode != 1)
      stop("POMDP is not time-dependent. There is only a single episode.")
    
    reward <-  model[[field]]
  }
  
  if (is.null(reward))
    stop(
      "Field ",
      field,
      " is not available. Parsing some fields is not implemented for models read with read_POMDP!"
    )
  
  # no reward available (e.g., for reading POMDP files)
  if (is.null(reward)) {
    warning(
      "Reward is not specified in the model description (e.g., when a POMDP model file is read)."
    )
    return (NULL)
  }
  
  
  if (is.data.frame(reward)) {
    reward[[1]] <- .get_names(reward[[1]], actions)
    reward[[2]] <- .get_names(reward[[2]], states)
    reward[[3]] <- .get_names(reward[[3]], states)
    reward[[4]] <- .get_names(reward[[4]], observations)
    reward <- sapply(
      actions,
      FUN = function(a)
        sapply(
          states,
          FUN = function(s) {
            if(!is.null(observations)) {
              .df2matrix(model,
                reward[(reward$action == a | is.na(reward$action)) &
                    (reward$start.state == s |
                        is.na(reward$start.state)), 3:5],
                from = "states", to = "observations")
            }else{
              ## MDPs have no observations
              .df2vector(model,
                reward[(reward$action == a | is.na(reward$action)) &
                    (reward$start.state == s |
                        is.na(reward$start.state)), c(3,5)],
                from = "states")
            }
          },
          simplify = FALSE,
          USE.NAMES = TRUE
        ),
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    ## translate from function
  } else if (is.function(reward)) {
    reward <- Vectorize(reward)
    reward <- sapply(
      actions,
      FUN = function(a)
        sapply(
          states,
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
            p
          },
          simplify = FALSE,
          USE.NAMES = TRUE
        ),
      simplify = FALSE,
      USE.NAMES = TRUE
    )
  }
  
  ### FIXME: missing. Also version without observations!
  ## translate from list of matrices (fix names, and deal with "identity", et al)
  ##} else if (is.list(reward)) {
  ##}
   
  if(!is.null(action) && length(action) == 1) 
    reward <- reward[[1]]
  
  if(!is.null(start.state) && length(start.state) == 1) 
    reward <- reward[[1]]
  
  reward
}
