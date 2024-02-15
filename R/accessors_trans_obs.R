# Accessor Functions for transitions and observations
#
# Representations:
# Default:
# * Sparse (list):
#     Trans: A action list -> start.state x end.state sparse matrix
#     Obs: A action list -> end.state x observation sparse matrix
#
# Others
# * Dense (list): Same as sparse with dense matrices
# * df: A data.frame with value
# * A function can be converted to a list
#
# sparse = NULL translates functions/data frames/strings
#
value_matrix <-
  function(x,
           field,
           action = NULL,
           row = NULL,
           col = NULL,
           episode = NULL,
           epoch = NULL,
           sparse = NULL) {
    ## action list of s x s matrices
    
    if (is.null(episode)) {
      if (is.null(epoch))
        episode <- 1L
      else
        episode <- epoch_to_episode(x, epoch)
    }
    
    if (.is_timedependent_field(x, field))
      value <- x[[field]][[episode]]
    else
      value <-  x[[field]]
    
    # convert functions
    if (is.function(value)) {
      # shortcut for a single value
      if (!is.null(action) && !is.null(row) && !is.null(col))
          return(value(action, row, col))

      return(function2value(x, field, value, action, row, col, sparse))
    }
    
    # data.frame
    if (is.data.frame(value)) {
      return(df2value(value, action, row, col, sparse))
    }
    
    # we have a list of matrices
    # subset
    list2value(x, field, value, action, row, col, sparse)
  }

#' @include accessors.R
#' @rdname accessors
#' @export
transition_matrix <-
  function(x,
           action = NULL,
           start.state = NULL,
           end.state = NULL,
           episode = NULL,
           epoch = NULL,
           sparse = FALSE) {
    value_matrix(x,
                 "transition_prob",
                 action,
                 start.state,
                 end.state,
                 episode,
                 epoch,
                 sparse)
    
  }

#' @rdname accessors
#' @export
transition_val <-
  function(x,
           action,
           start.state,
           end.state,
           episode = NULL,
           epoch = NULL) {
    #warning("transition_val is deprecated. Use reward_matrix instead!")
    value_matrix(x,
                 "transition_prob",
                 action,
                 start.state,
                 end.state,
                 episode,
                 epoch)
  }

#' @include accessors.R
#' @rdname accessors
#' @export
observation_matrix <-
  function(x,
           action = NULL,
           end.state = NULL,
           observation = NULL,
           episode = NULL,
           epoch = NULL,
           sparse = FALSE) {
    value_matrix(x,
                 "observation_prob",
                 action,
                 end.state,
                 observation,
                 episode,
                 epoch,
                 sparse)
    
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
    #warning("observation_val is deprecated. Use reward_matrix instead!")
    value_matrix(x,
                 "observation_prob",
                 action,
                 end.state,
                 observation,
                 episode,
                 epoch)
  }


### this just subsets the matrix list
list2value <-
  function(x,
           field,
           m,
           action = NULL,
           row = NULL,
           col = NULL,
           sparse = NULL) {
    actions <- x$actions
    rows <- x$states
    if (field == "transition_prob")
      cols <- x$states
    else
      ### obs
      cols <- x$observations
    
    ## convert from character
    .fix <- function(mm, sparse) {
      if (is.character(mm)) {
        mm <- switch(
          mm,
          identity = {
            if (is.null(sparse) || sparse)
              Matrix::Diagonal(length(rows))
            else
              diag(length(rows))
          },
          uniform = matrix(
            1 / length(cols),
            nrow = length(rows),
            ncol = length(cols)
          )
        )
        
        dimnames(mm) <- list(rows, cols)
      }
      .sparsify(mm, sparse)
    }
    
    if (is.null(action)) {
      m <- lapply(m, .fix, sparse = sparse)
      return(m)
    }
    
    m <- .fix(m[[action]], sparse)
    
    if (is.null(row) && is.null(col))
      return(m)
    
    if (is.null(row))
      row <- rows
    if (is.null(col))
      col <- cols
    
    return(m[row, col])
  }


df2value <-
  function(df,
           action = NULL,
           row = NULL,
           col = NULL,
           sparse = FALSE) {
    actions <- levels(df$action)
    rows <- levels(df[[2L]])
    cols <- levels(df[[3L]])
    
    if (is.null(action)) {
      l <- sapply(
        actions,
        FUN = function(a) {
          .sparsify(df2value(df, a), sparse = sparse)
        },
        simplify = FALSE
      )
      
      return(l)
    }
    
    if (is.null(col) && is.null(row))  {
      # matrix
      df <-
        df[(is.na(df$action) | df$action == action), , drop = FALSE]
      
      m <-
        matrix(
          0,
          nrow = length(rows),
          ncol = length(cols),
          dimnames = list(rows, cols)
        )
      
      for (i in seq_len(nrow(df))) {
        r <- df[[2L]][i]
        if (is.na(r))
          r <- rows
        
        c <- df[[3L]][i]
        if (is.na(c))
          c <- cols
        
        m[r, c] <- df$probability[i]
      }
      
      m <- .sparsify(m, sparse)
      return(m)
    }
    
    if (is.null(col)) {
      # row vector
      if (is.numeric(row))
        row <- rows[row]
      df <- df[(is.na(df$action) | df$action == action) &
                 (is.na(df[[2L]]) |
                    df[[2L]] == row), , drop = FALSE]
      
      v <-
        structure(numeric(length(cols)), names = cols)
      
      for (i in seq_len(nrow(df))) {
        c <- df[[3L]][i]
        if (is.na(c))
          c <- cols
        
        v[c] <- df$probability[i]
      }
      
      return(v)
    }
    
    if (is.null(row)) {
      if (is.numeric(col))
        col <- cols[col]
      # row vector
      df <- df[(is.na(df$action) | df$action == action) &
                 (is.na(df[[2L]]) |
                    df[[2L]] == col), , drop = FALSE]
      
      v <-
        structure(numeric(length(rows)), names = rows)
      
      for (i in seq_len(nrow(df))) {
        r <- df[[2L]][i]
        if (is.na(r))
          r <- rows
        
        v[r] <- df$probability[i]
      }
      
      return(v)
    }
    
    # value
    if (is.numeric(row))
      row <- rows[row]
    if (is.numeric(col))
      col <- cols[col]
    
    val <- df$probability[(is.na(df$action) | df$action == action) &
                            (is.na(df[[2L]]) |
                               df[[2L]] == row) &
                            (is.na(df[[3L]]) |
                               df$end.state == col)]
    
    if (length(val) == 0L)
      return(0)
    
    return(tail(val, 1L))
  }

function2value <- function(x,
                           field,
                           f,
                           action,
                           row,
                           col,
                           sparse = FALSE) {
  if (length(action) == 1L &&
      length(row) == 1L &&
      length(col) == 1L)
    return(f(action, row, col))
  
  # TODO: we could make access faster
  
  f <- Vectorize(f)
  actions <- x$actions
  rows <- x$states
  if (field == "transition_prob")
    cols <- x$states
  else
    ### obs
    cols <- x$observations
  
  m <- sapply(
    actions,
    FUN = function(a) {
      p <- outer(
        rows,
        cols,
        FUN = function(r, c)
          f(a,
            r,
            c)
      )
      dimnames(p) <- list(rows, cols)
      .sparsify(p, sparse)
    },
    simplify = FALSE
  )
  
  list2value(x, field, m,
             action,
             row,
             col,
             sparse = NULL)
}

#' @example
#' library(pomdp)
#' data(Tiger)
#' transition_matrix(Tiger)
#' transition_matrix(Tiger, sparse = TRUE)
#' transition_matrix(Tiger, sparse = FALSE)
#' transition_matrix(Tiger, "listen")
#' transition_matrix(Tiger, "listen", "tiger-left")
#'
