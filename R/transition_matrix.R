#' Extract the Transition, Observation or Reward Matrices from a POMDP
#' 
#' Converts the description of transition probabilities and observation
#' probabilities in a POMDP into a list of matrices, one for each action.
#' Rewards are converted into a list (actions) of lists (start states) of
#' matrices.
#' 
#' 
#' @aliases transition_matrix observation_matrix reward_matrix
#' @param x A POMDP object.
#' @param episode Episode used for time-dependent POMDPs (see
#' \code{\link{POMDP}}).
#' @return A list or a list of lists of matrices.
#' @author Michael Hahsler
#' @seealso \code{\link{POMDP}}
#' @examples
#' 
#' data("Tiger")
#' 
#' # transition matrices for each action in the from states x states
#' transition_matrix(Tiger)
#' 
#' # observation matrices for each action in the from states x observations
#' observation_matrix(Tiger)
#' 
#' # reward matrices for each matrix and (start) state in 
#' # the form (end) state x observation
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
#' @export
transition_matrix <- function(x, episode = 1)
  .translate_probabilities(x, field = "transition_prob", from = "states", to = "states", 
    episode = episode)

observation_matrix <- function(x, episode = 1) 
  .translate_probabilities(x, field = "observation_prob", from = "states", to = "observations", 
    episode = episode)

reward_matrix <- function(x, episode = 1) 
  .translate_reward(x, episode = episode)

# translate different specifications of transitions, observations and rewards
# into a list of matrices 

# df needs to have 3 columns: from, to, and val
.df2matrix <- function(model, df, from = "states", to = "observations"){
  from <- model$model[[from]]
  to <- model$model[[to]]
  m <- matrix(0, nrow = length(from), ncol = length(to), 
    dimnames = list(from, to))
  
  for(i in 1:nrow(df)){
    if(df[i, 1] == "*" && df[i, 2] == "*")
      m[] <- df[i, 3]
    else if (df[i, 1] == "*")
      m[, df[i, 2]] <- df[i, 3]
    else if (df[i, 2] == "*")
      m[df[i, 1], ] <- df[i, 3]
    else 
      m[df[i, 1], df[i, 2]] <- df[i, 3]
  }
  
  m
}

### helpers
.translate_probabilities <- function(model, 
  field = "transition_prob", from = "states", to = "states", episode = 1) {
  
  actions <- as.character(model$model$actions)
  
  ## episodes are for time-dependent POMDPs 
  if(.timedependent_POMDP(model)) {
    episode <- as.integer(episode)
    if(episode < 1L || episode > length(model$model$horizon)) stop("Requested episode does not exit.")
    
    if(.is_timedependent(model, field)) prob <- model$model[[field]][[episode]]
    else prob <-  model$model[[field]]
    
  } else  prob <-  model$model[[field]]
  
  if(is.null(prob)) stop("Inconsistent POMDP definition. Requested episode does not exit.")
  
  ## translate dataframes
  if(is.data.frame(prob)) {
    prob <- sapply(actions, function(a) {
      .df2matrix(model, 
        prob[(prob$action == a | prob$action == "*"), 2:4],
        from = from, to = to)
    }, simplify = FALSE, USE.NAMES = TRUE)
    
  } else if(is.list(prob)) {
    if(is.null(names(prob))) names(prob) <- actions
    else prob <- prob[actions]
    
    from <- as.character(model$model[[from]])
    to <- as.character(model$model[[to]])
    
    prob <- lapply(prob, FUN = function(tr) {
      if(is.character(tr)) {
        tr <- switch(tr, 
          identity = diag(1, nrow = length(from), ncol = length(to)),
          uniform = matrix(1/length(to), 
            nrow = length(from), ncol = length(to)))
      }
      
      if(!is.matrix(tr)) stop("Probabilities cannot be converted to matrix.")
      if(is.null(dimnames(tr))) dimnames(tr) <- list(from, to)
      else tr <- tr[from, to]
      
      tr
    })
  } else stop("Unknown transition/observation matrix format.")
  prob 
}

.translate_reward <- function(model, episode = 1) {
  
  actions <- as.character(model$model$actions)
  states <- as.character(model$model$states)

  field <- "reward"
  ## episodes are for time-dependent POMDPs 
  if(.timedependent_POMDP(model)) {
    episode <- as.integer(episode)
    if(episode < 1L || episode > length(model$model$horizon)) stop("Requested episode does not exit.")
    
    if(.is_timedependent(model, field)) prob <- model$model[[field]][[episode]]
    else prob <-  model$model[[field]]
    
  } else { 
    if(episode != 1) stop("POMDP is not time-dependent. There is only a single episode.")
    
    prob <-  model$model[[field]]
  }
    
  if(is.null(prob)) stop("Inconsistent POMDP definition. Requested episode does not exit.")
  
  reward <- prob
  
  # no reward avaiable (e.g., for reading POMDP files)
  if(is.null(reward)) {
    warning("Reward is not specified in the model description (e.g., when a POMDP model file is read).")
    return (NULL)
  }
  
  for(i in 1:4) reward[[i]] <- as.character(reward[[i]])
  
  if(is.data.frame(reward)) {
    reward <- sapply(actions, FUN = function(a) 
      sapply(states, FUN = function(s) {
        .df2matrix(model, 
          reward[(reward$action == a | reward$action == "*") & 
              (reward$start.state == s | reward$start.state == "*"), 3:5],
          from = "states", to = "observations")
      }, simplify = FALSE, USE.NAMES = TRUE
      ), simplify = FALSE, USE.NAMES = TRUE
    )
  }
  
  reward
}
