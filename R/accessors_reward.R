# Accessor Functions for Rewards
#
# Representations:
# Default:
# * Sparse (df): A data.frame with: action start.state end.state observation value
# 
# Others:
# * Dense (list): A action list -> start.state list -> end.state x observation matrix
# * A function can be converted to a list


### Note: Sparse is a data.frame for rewards
#' @include accessors.R
#' @rdname accessors
#' @export
reward_matrix <-
  function(x,
           action = NULL,
           start.state = NULL,
           end.state = NULL,
           observation = NULL,
           episode = NULL,
           epoch = NULL,
           sparse = FALSE) {
    if (inherits(x, "MDP") && !is.null(observation))
      stop("Observations can not be specified for MDPs!")
    ## action list of s' x o matrices
    ## action list of s list of s' x o matrices
    ## if not observations are available then it is a s' vector
    if (is.null(episode)) {
      if (is.null(epoch))
        episode <- 1L
      else
        episode <- epoch_to_episode(x, epoch)
    }
    
    if (.is_timedependent_field(x, "reward"))
      reward <- x[["reward"]][[episode]]
    else
      reward <-  x[["reward"]]
    
    if (is.null(action) && (!is.null(start.state) || !is.null(end.state) || !is.null(observation)))
      stop("action needs to be specified!")
    if (!is.null(action) && is.null(start.state) && (!is.null(end.state) || !is.null(observation)))
      stop("start.state needs to be specified!")
    
    # convert functions first
    if (is.function(reward)) {
      # shortcut for a single value
      if (!is.null(action) && !is.null(start.state) && !is.null(end.state)) {
        if(inherits(x, "MDP"))
          return(reward(action, start.state, end.state))
        else if (!is.null(observation))
          return(reward(action, start.state, end.state, observation))
      }
        
      reward <- reward_function2list(reward, x)
    }
    
    # return as is
    if(is.null(sparse))
      return(reward)
    
    # data.frame
    if (is.data.frame(reward)) {
      if (sparse)
        return(reward)
      else
        return(reward_df2value(reward, action, start.state, end.state, observation))
    }
    
    # we have a list of list of  matrices
    if (sparse)
      return(reward_list2df(reward))
    
    # subset
    reward_list2value(reward, action, start.state, end.state, observation)
  }

#' @rdname accessors
#' @export
reward_val <-
  function(x,
           action,
           start.state,
           end.state = NULL,
           observation = NULL,
           episode = NULL,
           epoch = NULL) {
    #warning("reward_val is deprecated. Use reward_matrix instead!")
    reward_matrix(x,
                  action,
                  start.state,
                  end.state,
                  observation,
                  episode,
                  epoch,
                  sparse = FALSE)
    
  }

# used internally
.max_abs_reward <- function(x, episode = NULL,
                            epoch = NULL) {
  r <-
    reward_matrix(x,
                  episode = episode,
                  epoch = epoch,
                  sparse = NULL)
  
  if (is.data.frame(r)) {
    rew <- r$value
  } else {
    # list of list of matrices
    rew <- unlist(x$reward, recursive = TRUE)
  }
  
  rew[rew == -Inf] <- NA
  max(abs(rew), na.rm = TRUE)
}


### Tests
#library(pomdp)
# data(RussianTiger)
#
# df <- RussianTiger$reward
# df
#
# reward_df2value(df, "nothing", "s_tiger-left")
# reward_df2value(df, "nothing", "s_tiger-left", "s_tiger-left")
# reward_df2value(df, "nothing", "s_tiger-left", observation = "s_tiger-left")
# reward_df2value(df, "nothing", "s_tiger-left", "s_tiger-left", "s_tiger-left")
# reward_df2value(df, 1, 1, 1, 1)
#
# reward_df2list(df)
#
# x <- reward_df2list(df)
# reward_list2value(x, "nothing", "s_tiger-left")
# reward_list2value(x, "nothing", "s_tiger-left", "s_tiger-left")
# reward_list2value(x, "nothing", "s_tiger-left", observation = "o_tiger-left")
# reward_list2value(x, "nothing", "s_tiger-left", "s_tiger-left", "o_tiger-left")
# reward_list2value(x, 1, 1, 1, 1)
#
# f <- function(action,
#          start.state,
#          end.state,
#          observation) {
#
#   if (action == "nothing")
#     return(-Inf)
#   return(-1)
# }
#
# reward_function2list(f, RussianTiger)


### NOTE: this does not check if the actions/states/observations are valid names!
reward_df2value <-
  function(df,
           action = NULL,
           start.state = NULL,
           end.state = NULL,
           observation = NULL,
           sparse = FALSE) {

    actions <- levels(df$action)
    states <- levels(df$end.state)
    
    observations <- levels(df$observation)
    # MDP has no observations
    if (is.null(observations)) {
      is_MDP <- TRUE
      observations <- NA
      observation <- 1L
    } else {
      is_MDP <- FALSE
    }  
    
    if (is.null(action)) {
      l <- sapply(
        actions,
        FUN = function(a) {
          sapply(
            states,
            FUN = function(s) {
              .sparsify(reward_df2value(df, a, s), sparse = sparse)
            },
            simplify = FALSE
          )
        },
        simplify = FALSE
      )
      return(l)
    }
    
    if (is.null(start.state)) {
      l <-
        sapply(
          states,
          FUN = function(s) {
            .sparsify(reward_df2value(df, action, s), sparse = sparse)
          },
          simplify = FALSE
        )
      
      return(l)
    }
    
    if (is.numeric(action))
      action <- actions[action]
    if (is.numeric(start.state))
      start.state <- states[start.state]
    
    # cannot happen for MDP
    if (is.null(end.state) &&  is.null(observation)) {
      # matrix
      df <- df[(is.na(df$action) | df$action == action) &
                 (is.na(df$start.state) |
                    df$start.state == start.state), , drop = FALSE]
      
      m <-
        matrix(
          0,
          nrow = length(states),
          ncol = length(observations),
          dimnames = list(states, observations)
        )
      
      for (i in seq_len(nrow(df))) {
        e.s <- df$end.state[i]
        if (is.na(e.s))
          e.s <- states
        
        obs <- df$observation[i]
        if (is.na(obs))
          obs <- observations
        
        m[e.s, obs] <- df$value[i]
      }
      
      m <- .sparsify(m, sparse)
      
      return(m)
    }
    
    # cannot happen for MDP
    if (!is.null(end.state) && is.null(observation)) {
      # observation vector
      if (is.numeric(end.state))
        end.state <- states[end.state]
      df <- df[(is.na(df$action) | df$action == action) &
                 (is.na(df$start.state) |
                    df$start.state == start.state) &
                 (is.na(df$end.state) | df$end.state == end.state)
               , , drop = FALSE]
      
      v <-
        structure(numeric(length(observations)), names = observations)
      
      for (i in seq_len(nrow(df))) {
        obs <- df$observation[i]
        if (is.na(obs))
          obs <- observations
        
        v[obs] <- df$value[i]
      }
      
      return(v)
    }
    
    
    if (is.null(end.state) && !is.null(observation)) {
      # end.state vector
      if (is_MDP) {
        df <- df[(is.na(df$action) | df$action == action) &
                   (is.na(df$start.state) |
                      df$start.state == start.state)
                 , , drop = FALSE]
      } else {
        # POMDP
        if (is.numeric(observation))
          observation <- observations[observation]
        
        df <- df[(is.na(df$action) | df$action == action) &
                   (is.na(df$start.state) |
                      df$start.state == start.state) &
                   (is.na(df$observation) |
                      df$observation == observation)
                 , , drop = FALSE]
      }
      
      v <- structure(numeric(length(states)), names = states)
      
      for (i in seq_len(nrow(df))) {
        e.s <- df$end.state[i]
        
        if (is.na(e.s))
          e.s <- states
        
        v[e.s] <- df$value[i]
      }
      
      return(v)
      
    }
    
    # value
    if (is.numeric(end.state))
      end.state <- states[end.state]
    if (is_MDP)
      val <- df$value[(is.na(df$action) | df$action == action) &
                        (is.na(df$start.state) |
                           df$start.state == start.state) &
                        (is.na(df$end.state) |
                           df$end.state == end.state)]
    
    else
      val <- df$value[(is.na(df$action) | df$action == action) &
                        (is.na(df$start.state) |
                           df$start.state == start.state) &
                        (is.na(df$end.state) |
                           df$end.state == end.state) &
                        (is.na(df$observation) |
                           df$observation == observation)]
    
    if (length(val) == 0L)
      return(0)
    
    return(tail(val, 1L))
  }

reward_df2list <- function(df, sparse = FALSE)
  reward_df2value(df, sparse = sparse)

reward_list2df <- function(x) {
  actions <- names(x)
  states <- names(x[[1L]])
  observations <- colnames(x[[1L]][[1L]])
  
  reward_df <- data.frame()
  for (action in seq_along(actions)) {
    for (start.state in seq_along(states)) {
      for (observation in seq_along(observations)) {
        for (end.state in seq_along(states)) {
          reward_df <- rbind(reward_df,
                             R_(
                               action,
                               start.state,
                               end.state,
                               observation,
                               value = x[[action]][[start.state]][end.state, observation]
                             ))
        }
      }
    }
  }
  
  # use most common as the default
  # TODO: more simplifications
  tbl <- table(reward_df$value)
  most_common <- as.numeric(names(tbl[which.max(tbl)]))
  default_lines <- R_(value =  most_common)
  reward_df <-
    reward_df[reward_df$value != most_common, , drop = FALSE]
  
  reward_df <- rbind(default_lines, reward_df)
  ## TODO: MDP
  
  reward_df[["action"]] <-
    factor(reward_df[["action"]], labels = actions, levels = seq_along(actions))
  reward_df[["start.state"]] <-
    factor(reward_df[["start.state"]], labels = states, levels = seq_along(states))
  reward_df[["end.state"]] <-
    factor(reward_df[["end.state"]], labels = states, levels = seq_along(states))
  reward_df[["observation"]] <-
    factor(reward_df[["observation"]], labels = observations, levels = seq_along(observations))
  
  
  rownames(reward_df) <- NULL
  
  reward_df
}


reward_function2list <- function(f, model, sparse = FALSE) {
  actions <- model$actions
  states <- model$states
  observations <- model$observations
  
  f <- Vectorize(f)
  
  sapply(
    actions,
    FUN = function(a)
      sapply(
        states,
        FUN = function(s) {
          p <- outer(
            states,
            observations,
            FUN = function(end, o)
              f(
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


### this just subsets the matrix list
reward_list2value <-
  function(m,
           action = NULL,
           start.state = NULL,
           end.state = NULL,
           observation = NULL) {
    
    states <- names(m[[1L]])
    observations <- colnames(m[[1L]][[1L]])

    if (is.null(action))
      return(m)
    if (is.null(start.state))
      return(m[[action]])
    
    m <- m[[action]][[start.state]]
    
    if (is.null(end.state) &&  is.null(observation)) {
      # matrix
      return(m)
    }
    
    if (!is.null(end.state) && is.null(observation)) {
      # observation vector (drop is for MDP)
      return(m[end.state, , drop = TRUE])
    }
    
    if (is.null(end.state) && !is.null(observation)) {
      return(m[, observation])
    }
    
    # value
    return(m[end.state, observation])
  }
