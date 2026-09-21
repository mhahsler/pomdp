# Accessor Functions for transitions and observations
#
# Representations:
# Default:
# * Sparse (list):
#     Trans: An action list -> start.state x end.state sparse matrix
#     Obs: An action list -> end.state x observation sparse matrix
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
           sparse = NULL,
           trans_keyword = TRUE,
           drop = TRUE) {
    ## action list of s x s matrices

    .validate_scalar_logical(drop, "drop")
    episode <- .validate_episode_epoch(x, episode, epoch)
    action <- .match_model_value(action, x$actions, "action")
    row <- .match_model_value(
      row, x$states, if (field == "transition_prob") "start.state" else "end.state"
    )
    cols <- if (field == "transition_prob") x$states else x$observations
    col <- .match_model_value(
      col, cols, if (field == "transition_prob") "end.state" else "observation"
    )
    if (is.null(action) && (!is.null(row) || !is.null(col)))
      stop("action needs to be specified.", call. = FALSE)
    
    if (.is_timedependent_field(x, field))
      value <- x[[field]][[episode]]
    else
      value <-  x[[field]]
    
    # convert functions
    if (is.function(value)) {
      # shortcut for a single value
      if (!is.null(action) && !is.null(row) && !is.null(col)) {
        if (is.numeric(action)) action <- x$actions[action]
        if (is.numeric(row)) row <- x$states[row]
        if (field == "transition_prob")
          cols <- x$states
        else
          ### obs
          cols <- x$observations
        if (is.numeric(col)) col <- cols[col]
        result <- value(action, row, col)
        if (drop)
          return(result)
        return(matrix(result, 1L, 1L, dimnames = list(row, col)))
      }

      return(function2value(x, field, value, action, row, col, sparse, drop))
    }
    
    # data.frame
    if (is.data.frame(value)) {
      return(df2value(value, action, row, col, sparse, drop))
    }
    
    # we have a list of matrices
    # subset
    list2value(x, field, value, action, row, col, sparse, trans_keyword, drop)
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
           sparse = FALSE,
           trans_keyword = TRUE,
           drop = TRUE) {
    value_matrix(x,
                 "transition_prob",
                 action,
                 start.state,
                 end.state,
                 episode,
                 epoch,
                 sparse,
                 trans_keyword,
                 drop)
    
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
    .Deprecated("transition_matrix", package = "pomdp")
    value_matrix(x,
                 "transition_prob",
                 action,
                 start.state,
                 end.state,
                 episode,
                 epoch,
                 drop = TRUE)
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
           sparse = FALSE,
           trans_keyword = TRUE,
           drop = TRUE) {
    value_matrix(x,
                 "observation_prob",
                 action,
                 end.state,
                 observation,
                 episode,
                 epoch,
                 sparse,
                 trans_keyword,
                 drop)
    
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
    .Deprecated("observation_matrix", package = "pomdp")
    value_matrix(x,
                 "observation_prob",
                 action,
                 end.state,
                 observation,
                 episode,
                 epoch,
                 drop = TRUE)
  }


### this just subsets the matrix list
list2value <-
  function(x,
           field,
           m,
           action = NULL,
           row = NULL,
           col = NULL,
           sparse = NULL,
           trans_keyword = TRUE,
           drop = TRUE) {
    actions <- x$actions
    rows <- x$states
    if (field == "transition_prob")
      cols <- x$states
    else
      ### obs
      cols <- x$observations
    
    ## convert from character
    .fix <- function(mm, sparse, trans_keyword = TRUE) {
      if (is.character(mm)) {
        if (!trans_keyword)
          return(mm)
        
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
      m <- lapply(m, .fix, sparse = sparse, trans_keyword = trans_keyword)
      return(m)
    }
    
    m <- .fix(m[[action]], sparse, trans_keyword)
    
    if (is.null(row) && is.null(col))
      return(m)
    
    if (is.null(row))
      row <- rows
    if (is.null(col))
      col <- cols
    
    return(m[row, col, drop = drop])
  }


df2value <-
  function(df,
           action = NULL,
           row = NULL,
           col = NULL,
           sparse = FALSE,
           drop = TRUE) {
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
      
      if (drop)
        return(v)
      return(matrix(v, nrow = 1L, dimnames = list(row, cols)))
    }
    
    if (is.null(row)) {
      if (is.numeric(col))
        col <- cols[col]
      # row vector
      df <- df[(is.na(df$action) | df$action == action) &
                 (is.na(df[[3L]]) |
                    df[[3L]] == col), , drop = FALSE]
      
      v <-
        structure(numeric(length(rows)), names = rows)
      
      for (i in seq_len(nrow(df))) {
        r <- df[[2L]][i]
        if (is.na(r))
          r <- rows
        
        v[r] <- df$probability[i]
      }
      
      if (drop)
        return(v)
      return(matrix(v, ncol = 1L, dimnames = list(rows, col)))
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
      val <- 0
    else
      val <- tail(val, 1L)
    
    if (drop)
      return(val)
    matrix(val, 1L, 1L, dimnames = list(row, col))
  }

function2value <- function(x,
                           field,
                           f,
                           action,
                           row,
                           col,
                           sparse = FALSE,
                           drop = TRUE) {
  if (length(action) == 1L &&
      length(row) == 1L &&
      length(col) == 1L)
    return(if (drop) f(action, row, col) else
      matrix(f(action, row, col), 1L, 1L, dimnames = list(row, col)))
  
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
             sparse = NULL,
             drop = drop)
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
