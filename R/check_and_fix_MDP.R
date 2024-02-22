# Internal function to make sure the definition is complete and
# everything is in the right order and the right factors
check_and_fix_MDP <- function(x) {
  check_func <- function(x, func, name) {
    req_formals <- head(names(formals(func)),-1)
    if (!identical(names(formals(x)), req_formals))
      stop(name,
           " function needs formal arguments: ",
           paste(sQuote(req_formals), collapse = ", "))
  }
  
  check_df <- function(x, field, func) {
    req_columns <- names(formals(func))
    if (is.null(colnames(field)))
      colnames(field) <- req_columns
    
    if (!identical(colnames(field), req_columns))
      stop(
        "The ",
        deparse(substitute(field)),
        " data.frame needs columns named: ",
        paste(sQuote(req_columns), collapse = ", ")
      )
    
    # convert * to NA
    field[field == '*'] <- NA
    field <- type.convert(field, as.is = TRUE)
    
    for (i in grep("action", colnames(field))) {
      if (is.numeric(field[[i]]))
        field[[i]] <- x$actions[field[[i]]]
      field[[i]] <- factor(field[[i]], levels = x$actions)
    }
    
    for (i in grep("state", colnames(field))) {
      if (is.numeric(field[[i]]))
        field[[i]] <- x$states[field[[i]]]
      field[[i]] <- factor(field[[i]], levels = x$states)
    }
    for (i in grep("observation", colnames(field))) {
      if (is.numeric(field[[i]]))
        field[[i]] <- x$observations[field[[i]]]
      field[[i]] <- factor(field[[i]], levels = x$observations)
    }
    
    field
  }
  
  ### do the checking
  # expand states, actions and observations if only the number is given
  if (is.numeric(x$states) &&
      length(x$states) == 1L)
    x$states <- paste0("s", seq_len(x$states))
  
  if (is.numeric(x$actions) &&
      length(x$actions) == 1L)
    x$actions <- paste0("a", seq_len(x$actions))
  
  if (inherits(x, "POMDP")) {
    if (is.numeric(x$observations) &&
        length(x$observations) == 1L)
      x$observations <- paste0("o", seq_len(x$observations))
  }
  
  x$discount <- as.numeric(x$discount)
  if (length(x$discount) != 1L ||
      x$discount <= 0 || x$discount > 1)
    stop("discount has to be a single value in the range (0,1].")
  
  if (is.null(x$horizon))
    x$horizon <- Inf
  x$horizon <- as.numeric(x$horizon)
  if (any(x$horizon != floor(x$horizon)))
    stop("horizon needs to be an integer.")
  
  # start
  if (is.numeric(x$start) &&
      length(x$start) == length(x$states)) {
    if (!sum1(x$start))
      stop("The start probability vector does not add up to 1.")
    if (is.null(names(x$start)))
      names(x$start) <- x$states
    else
      x$start <- x$start[x$states]
  }
  if (any(is.na(x$start)))
    stop("start containes undefined start states.")
  if (is.character(x$start)) {
    if (!(identical(x$start, "uniform") || all(x$start %in% x$states)))
      stop(
        "when using characters for start, then it needs to be the keyword 'uniform' or a set of start states."
      )
  }
  
  if ((is.null(x$transition_prob) ||
       (inherits(x, "POMDP") &&
        is.null(x$observation_prob)) ||
       is.null(x$reward)) && is.null(x$problem))
    stop("transition_prob, observation_prob or reward can only miss if the field problem is set!")
  
  if (!is.null(x$transition_prob) &&
      !.is_timedependent_field(x, "transition_prob")) {
    # if we have matrices then check and add names
    if (is.function(x$transition_prob))
      check_func(x$transition_prob, T_, "transition_prob")
    #x$transition_prob <- transition_matrix(x)
    else if (is.data.frame(x$transition_prob))
      x$transition_prob <- check_df(x, x$transition_prob, T_)
    else {
      # action names and order
      if (is.null(names(x$transition_prob)))
        names(x$transition_prob) <- x$actions
      if (all(names(x$transition_prob) != x$actions))
        x$transition_prob <- x$transition_prob[x$actions]
      
      for (a in x$actions) {
        if (is.null(x$transition_prob[[a]]))
          stop("transition_prob for action ", a, " is missing!")
        if (is.matrix(x$transition_prob[[a]])) {
          if (!identical(dim(x$transition_prob[[a]]), c(length(x$states), length(x$states))))
            stop("transition_prob matrix for action ",
                 a,
                 ": has not the right dimensions!")
          if (!sum1(x$transition_prob[[a]]))
            stop("transition_prob matrix for action ",
                 a,
                 ": rows do not add up to 1!")
          if (is.null(dimnames(x$transition_prob[[a]])))
            dimnames(x$transition_prob[[a]]) <-
              list(x$states, x$states)
          else
            x$transition_prob[[a]][x$states, x$states]
        }
      }
    }
  }
  
  # time dependent checks
  if (!is.null(x$transition_prob) &&
      .is_timedependent_field(x, "transition_prob")) {
    # if we have matrices then check and add names
    for (e in seq_along(x$horizon)) {
      if (is.function(x$transition_prob[[e]]))
        check_func(x$transition_prob[[e]], T_, "transition_prob")
      #x$transition_prob[[e]] <- transition_matrix(x, episode = e)
      else if (is.data.frame(x$transition_prob[[e]]))
        x$transition_prob[[e]] <-
          check_df(x, x$transition_prob[[e]], T_)
      else {
        if (is.null(names(x$transition_prob[[e]])))
          names(x$transition_prob[[e]]) <- x$actions
        if (all(names(x$transition_prob[[e]]) != x$actions))
          x$transition_prob[[e]] <-
            x$transition_prob[[e]][x$actions]
        
        for (a in x$actions) {
          if (is.null(x$transition_prob[[e]][[a]]))
            stop("transition_prob for action ",
                 a,
                 " is missing in epoch ",
                 e,
                 "!")
          if (is.matrix(x$transition_prob[[e]][[a]])) {
            if (!identical(dim(x$transition_prob[[e]][[a]]), c(length(x$states), length(x$states))))
              stop(
                "transition_prob matrix for action ",
                a,
                " in epoch ",
                e,
                ": has not the right dimensions!"
              )
            if (!sum1(x$transition_prob[[e]][[a]]))
              stop(
                "transition_prob matrix for action ",
                a,
                " in epoch ",
                e,
                ": rows do not add up to 1!"
              )
            if (is.null(dimnames(x$transition_prob[[e]][[a]])))
              dimnames(x$transition_prob[[e]][[a]]) <-
                list(x$states, x$states)
            else
              x$transition_prob[[e]][[a]][x$states, x$states]
          }
        }
      }
    }
  }
  
  if (!is.null(x$reward) && !.is_timedependent_field(x, "reward")) {
    # MDP has no observations
    if (!inherits(x, "POMDP")) {
      R_ <- function(action = NA,
                     start.state = NA,
                     end.state = NA,
                     value)
        data.frame(
          action = action,
          start.state = start.state,
          end.state = end.state,
          value = as.numeric(value),
          stringsAsFactors = FALSE
        )
    }
    if (is.function(x$reward))
      check_func(x$reward, R_, "reward")
    #x$reward <- reward_matrix(x)
    
    if (is.data.frame(x$reward)) {
      x$reward <- check_df(x, x$reward, R_)
    }
  } else {
    if (is.null(names(x$reward)))
      names(x$reward) <- x$actions
    if (all(names(x$reward) != x$actions))
      x$reward <- x$reward[x$actions]
    
    for (a in x$actions) {
      if (is.null(x$reward[[a]]))
        stop("reward for action ", a, " is missing!")
      for (s in x$states) {
        if (is.null(x$reward[[a]][[s]]))
          stop("reward for action ",
               a,
               " and state ",
               s,
               " is missing!")
        if (is.matrix(x$reward[[a]][[s]])) {
          if (!identical(dim(x$reward[[a]][[s]]), c(length(x$states), length(x$observations))))
            stop(
              "reward matrix for action ",
              a,
              " and start.state ",
              s,
              ": has not the right dimensions!"
            )
          if (is.null(dimnames(x$reward[[a]][[s]])))
            dimnames(x$reward[[a]][[s]]) <-
              list(x$states, x$observations)
          else
            x$reward[[a]][[s]][x$states, x$observations]
        }
      }
    }
  }
  
  # time dependent checks
  if (!is.null(x$reward) &&
      .is_timedependent_field(x, "reward")) {
    for (e in seq_along(x$horizon)) {
      if (is.function(x$reward[[e]]))
        check_func(x$reward[[e]], R_, "reward")
      #x$reward[[e]] <- reward_matrix(x, episode = e)
      
      if (is.data.frame(x$reward[[e]]))
        x$reward[[e]] <- check_df(x, x$reward[[e]], R_)
      else {
        if (is.null(names(x$reward[[e]])))
          names(x$reward[[e]]) <- x$actions
        if (all(names(x$reward[[e]]) != x$actions))
          x$reward[[e]] <- x$reward[[e]][x$actions]
        
        for (a in x$actions) {
          if (is.null(x$reward[[e]][[a]]))
            stop("reward for action ", a, " in episode ", e, " is missing!")
          for (s in x$states) {
            if (is.null(x$reward[[e]][[a]][[s]]))
              stop("reward for action ",
                   a,
                   " and state ",
                   s,
                   " in episode ",
                   e,
                   " is missing!")
            if (is.matrix(x$reward[[e]][[a]][[s]])) {
              if (!identical(dim(x$reward[[e]][[a]][[s]]), c(length(x$states), length(x$observations))))
                stop(
                  "reward matrix for action ",
                  a,
                  " and start.state ",
                  s,
                  " in episode ",
                  e,
                  ": has not the right dimensions!"
                )
              if (is.null(dimnames(x$reward[[e]][[a]][[s]])))
                dimnames(x$reward[[e]][[a]][[s]]) <-
                  list(x$states, x$observations)
              else
                x$reward[[e]][[a]][[s]][x$states, x$observations]
            }
          }
        }
      }
    }
  }
  
  ### check terminal values (only POMDP for now)
  if (inherits(x, "POMDP") && !is.null(x$terminal_values)) {
    if (length(x$terminal_values) != 1L &&
        length(x$terminal_values) != length(x$states) &&
        (is.matrix(x$terminal_values) &&
         ncol(x$terminal_values) != length(x$states)))
      stop("Terminal values are not in the right format.")
  }
  
  ### check solution
  if (inherits(x, "POMDP") && !is.null(x$solution)) {
    # alpha
    if (any(sapply(x$solution$alpha, ncol) != length(x$states)))
      stop("Alpha vectors do not have the right dimension.")
    
    # pg
    x$solution$pg <- lapply(
      x$solution$pg,
      FUN = function(y) {
        y$action <- factor(y$action, levels = x$actions)
        y
      }
    )
  }
  
  if (inherits(x, "POMDP") &&
      !is.null(x$observation_prob) &&
      !.is_timedependent_field(x, "observation_prob")) {
    if (is.function(x$observation_prob))
      check_func(x$observation_prob, O_, "observation_prob")
    #x$observation_prob <- observation_matrix(x)
    
    else if (is.data.frame(x$observation_prob))
      x$observation_prob <-
        check_df(x, x$observation_prob, O_)
    else {
      if (is.null(names(x$observation_prob)))
        names(x$observation_prob) <- x$actions
      if (all(names(x$observation_prob) != x$actions))
        x$observation_prob <- x$observation_prob[x$actions]
      
      for (a in x$actions) {
        if (is.null(x$observation_prob[[a]]))
          stop("observation_prob for action ",
               a,
               " is missing!")
        if (is.matrix(x$observation_prob[[a]])) {
          if (!identical(dim(x$observation_prob[[a]]), c(length(x$states), length(x$observations))))
            stop("observation_prob matrix for action ",
                 a,
                 ": has not the right dimensions!")
          if (!sum1(x$observation_prob[[a]]))
            stop("observation_prob matrix for action ",
                 a,
                 ": rows do not add up to 1!")
          
          if (is.null(dimnames(x$observation_prob[[a]])))
            dimnames(x$observation_prob[[a]]) <-
              list(x$states, x$observations)
          else
            x$observation_prob[[a]][x$states, x$observations]
        }
      }
    }
  }
  
  ## time dependent checks
  if (inherits(x, "POMDP") &&
      !is.null(x$observation_prob) &&
      .is_timedependent_field(x, "observation_prob")) {
    for (e in seq_along(x$horizon)) {
      if (is.function(x$observation_prob[[e]]))
        check_func(x$observation_prob[[e]], O_, "observation_prob")
      #x$observation_prob <- observation_matrix(x, episode = e)
      
      if (is.data.frame(x$observation_prob[[e]]))
        x$observation_prob[[e]] <-
          check_df(x, x$observation_prob[[e]], O_)
      else {
        if (is.null(names(x$observation_prob[[e]])))
          names(x$observation_prob[[e]]) <- x$actions
        if (all(names(x$observation_prob[[e]]) != x$actions))
          x$observation_prob[[e]] <-
            x$observation_prob[[e]][x$actions]
        
        for (a in x$actions) {
          if (is.null(x$observation_prob[[e]][[a]]))
            stop("observation_prob for action ", a, " is missing!")
          if (is.matrix(x$observation_prob[[e]][[a]])) {
            if (!identical(dim(x$observation_prob[[e]][[a]]), c(length(x$states), length(x$observations))))
              stop("observation_prob matrix for action ",
                   a,
                   ": has not the right dimensions!")
            if (!all(rowSums(x$observation_prob[[e]][[a]]) == 1))
              stop("observation_prob matrix for action ",
                   a,
                   ": rows do not add up to 1!")
            
            if (is.null(dimnames(x$observation_prob[[e]][[a]])))
              dimnames(x$observation_prob[[e]][[a]]) <-
                list(x$states, x$observations)
            else
              x$observation_prob[[e]][[a]][x$states, x$observations]
          }
        }
      }
    }
  }


x
}
