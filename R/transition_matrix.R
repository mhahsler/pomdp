#' Extract the Transition, Observation or Reward Matrices from a POMDP
#'
#' Converts the description of transition probabilities and observation
#' probabilities in a POMDP into a list of matrices.
#'
#' See Details section in [POMDP] for details.
#'
#' @aliases transition_matrix observation_matrix reward_matrix
#' @param x A [POMDP] object.
#' @param episode Episode used for time-dependent POMDPs ([POMDP]).
#' @return A list or a list of lists of matrices.
#' @author Michael Hahsler
#' @seealso [POMDP]
#' @examples
#' data("Tiger")
#'
#' # List of |A| transition matrices. One per action in the from states x states
#' Tiger$model$transition_prob
#' transition_matrix(Tiger)
#'
#' # List of |A| observation matrices. One per action in the from states x observations
#' Tiger$model$observation_prob
#' observation_matrix(Tiger)
#'
#' # List of list of reward matrices. 1st level is action and second level is the
#' #  start state in the form end state x observation
#' Tiger$model$reward
#' reward_matrix(Tiger)
#'
#' # Visualize transition matrix for action 'open-left'
#' library("igraph")
#' g <- graph_from_adjacency_matrix(transition_matrix(Tiger)$"open-left", weighted = TRUE)
#' edge_attr(g, "label") <- edge_attr(g, "weight")
#'
#' igraph.options("edge.curved" = TRUE)
#' plot(g, layout = layout_on_grid, main = "Transitions for action 'open=left'")
#'
#' ## Use a function for the Tiger transition model
#' trans <- function(end.state, start.state, action) {
#'   ## listen has an identity matrix
#'   if(action == 'listen')
#'     if(end.state == start.state) return(1)
#'     else return(0)
#'
#'   # other actions have a uniform distribution
#'   return(1/2)
#' }
#'
#' Tiger$model$transition_prob <- trans
#' transition_matrix(Tiger)
#' @export
transition_matrix <- function(x, episode = 1)
  .translate_probabilities(
    x,
    field = "transition_prob",
    from = "states",
    to = "states",
    episode = episode
  )

#' @rdname transition_matrix
#' @export
observation_matrix <- function(x, episode = 1)
  .translate_probabilities(
    x,
    field = "observation_prob",
    from = "states",
    to = "observations",
    episode = episode
  )

#' @rdname transition_matrix
#' @export
reward_matrix <- function(x, episode = 1)
  .translate_reward(x, episode = episode)

# translate different specifications of transitions, observations and rewards
# into a list of matrices

# df needs to have 3 columns: from, to, and val
.df2matrix <-
  function(model, df, from = "states", to = "observations") {
    from <- as.character(model$model[[from]])
    to <- as.character(model$model[[to]])
    
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
      if (df[i, 1] == "*" && df[i, 2] == "*")
        m[] <- df[i, 3]
      else if (df[i, 1] == "*")
        m[, df[i, 2]] <- df[i, 3]
      else if (df[i, 2] == "*")
        m[df[i, 1],] <- df[i, 3]
      else
        m[df[i, 1], df[i, 2]] <- df[i, 3]
    }
    
    m
  }

### helpers
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
  sparse = TRUE) {
  actions <- model$model$actions
  
  ## episodes are for time-dependent POMDPs
  if (.timedependent_POMDP(model)) {
    episode <- as.integer(episode)
    if (episode < 1L ||
        episode > length(model$model$horizon))
      stop("Requested episode does not exit in horizon specification.")
    
    if (.is_timedependent(model, field)) {
      prob <- model$model[[field]][[episode]]
      if (is.null(prob))
        stop(
          "Inconsistent POMDP definition. Requested episode does not exit in field ",
          field,
          "."
        )
    } else
      prob <-  model$model[[field]]
  } else
    prob <-  model$model[[field]]
  
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
        prob[(prob$action == a | prob$action == "*"), 2:4],
        from = from, to = to)
    }, simplify = FALSE, USE.NAMES = TRUE)
    
    ## translate from function
  } else if (is.function(prob)) {
    prob <- Vectorize(prob)
    prob <- sapply(actions, function(a) {
      p <- outer(
        model$model[[from]],
        model$model[[to]],
        FUN = function(from, to)
          prob(a,
            from,
            to)
      )
      dimnames(p) <- list(model$model[[from]], model$model[[to]])
      p
    }, simplify = FALSE, USE.NAMES = TRUE)
    
    ## translate from list of matrices (fix names, and deal with "identity", et al)
  } else if (is.list(prob)) {
    if (is.null(names(prob)))
      names(prob) <- actions
    else
      prob <- prob[actions]
    
    from <- as.character(model$model[[from]])
    to <- as.character(model$model[[to]])
    
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
        if (is.null(dimnames(tr)))
          dimnames(tr) <- list(from, to)
        
        if (is.null(prob))
          stop(
            "Field ",
            field,
            " is not available. Parsing some fields is not implemented for models read with read_POMDP!"
          )
        else
          tr <- tr[from, to]
        
        tr
      }
    )
  } else
    stop("Unknown transition/observation matrix format.")
  prob
}


## reward is action -> start.state -> end.state x observation
.translate_reward <- function(model, episode = 1) {
  actions <- model$model$actions
  states <- model$model$states
  observations <- model$model$observations
  
  field <- "reward"
  ## episodes are for time-dependent POMDPs
  if (.timedependent_POMDP(model)) {
    episode <- as.integer(episode)
    if (episode < 1L ||
        episode > length(model$model$horizon))
      stop("Requested episode does not exit.")
    
    if (.is_timedependent(model, field))
      reward <- model$model[[field]][[episode]]
    else
      reward <-  model$model[[field]]
    
  } else {
    if (episode != 1)
      stop("POMDP is not time-dependent. There is only a single episode.")
    
    reward <-  model$model[[field]]
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
            .df2matrix(model,
              reward[(reward$action == a | reward$action == "*") &
                  (reward$start.state == s |
                      reward$start.state == "*"), 3:5],
              from = "states", to = "observations")
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
  
  reward
}
