# FIXME: we should use pomdp:::round_stochastic here!

format_fixed <- function(x, digits = 7, debug = "unknown") {
  if (is.null(x))
    stop("missing field ", debug)
  
  if (is.vector(x)) {
    if (!is.numeric(x))
      stop("Write_POMDP expects numbers, but got: ", dQuote(paste(x, collapse = ", ")))
    paste(sprintf(paste0("%.", digits, "f"), x), collapse = " ")
  } else if (is.matrix(x)) {
    paste(apply(x, MARGIN = 1, format_fixed, digits = digits), collapse = "\n")
  } else
    stop("formating not implemented for ", class(x), " in field ", debug)
}

#' Read and write a POMDP Model to a File in POMDP Format
#'
#' Reads and write a POMDP file suitable for the `pomdp-solve` program.
#'
#' [POMDP] objects read from a POMDP file have an extra element called `problem` which contains the original
#' POMDP specification. **The original specification is directly used by external solvers.** In addition, the file
#' is parsed using an experimental POMDP file parser. The parsed information can be used with auxiliary functions
#' in this package that use fields like the transition matrix, the observation matrix and the reward structure.
#'
#' @family POMDP
#'
#' @param x an object of class [POMDP].
#' @param digits precision for writing numbers (digits after the decimal
#' point).
#' @param file a file name. `read_POMDP()` also accepts [connections] including URLs.
#' @return `read_POMDP()` returns a [POMDP] object.
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @references POMDP solver website: https://www.pomdp.org
#' @keywords IO
#' @examples
#' data(Tiger)
#'
#' ## show the POMDP file that would be written.
#' write_POMDP(Tiger, file = stdout())
#' @export
write_POMDP <- function(x, file, digits = 7) {
  if (!inherits(x, "POMDP"))
    stop("model needs to be a POMDP model use POMDP()!")
  
  x <- check_and_fix_MDP(x)
  
  with(x,
    {
      number_of_states        <- length(states)
      number_of_observations  <- length(observations)
      number_of_actions       <- length(actions)
      
      # we only support rewards and not cost
      values <- "reward"
      
      ## TODO: we currently convert function to matrix
      if (is.function(transition_prob))
        transition_prob <- transition_matrix(x, sparse = FALSE)
      if (is.function(observation_prob))
        observation_prob <- observation_matrix(x, sparse = FALSE)
      if (is.function(reward))
        reward <- reward_matrix(x, sparse = FALSE)
      
      ### POMDP file
      code <-  paste0(
        "# POMDP File: ",
        name,
        "\n",
        "# Produced with R package pomdp (created: ",
        date(),
        ")\n",
        "\n",
        "discount: ",
        format(discount, digits = digits),
        "\n",
        "values: ",
        values,
        "\n",
        "states: ",
        paste(states, collapse = " "),
        "\n",
        "actions: ",
        paste(actions, collapse = " "),
        "\n",
        "observations: ",
        paste(observations, collapse = " "),
        "\n"
      )
      
      ### starting beliefs
      if (!is.null(start)) {
        ## if the starting beliefs are given by enumerating the probabilities for each state
        if (is.numeric(start)) {
          if (length(start) == length(states) && sum1(start)) {
            code <-
              paste0(code,
                "start: ",
                format_fixed(start, digits, "start"),
                "\n")
          } else {
            ## this should be indices (pomdp_solve starts with 0)
            start_ids <- as.integer(abs(start)) - 1L
            if (all(start < 0))
              code <-
                paste0(code,
                  "start include: ",
                  paste(start_ids, collapse = " "),
                  "\n")
            if (all(start > 0))
              code <-
                paste0(code,
                  "start exclude: ",
                  paste(start_ids, collapse = " "),
                  "\n")
            else
              stop(
                "Illegal specification of start. ",
                "State ids start with 1 need to be all positive or negative (for exclusion)."
              )
          }
          
        } else if (is.character(start))
          ## if the starting beliefs are given by a uniform distribution over all states
          if (length(start) == 1 && start[1] == "uniform") {
            code <- paste0(code, "start: ", paste(start, collapse = " "), "\n")
            
          } else if (start[1] != "-")
            code <-
              paste0(code, "start include: ", paste(start, collapse = " "), "\n")
          else
            code <-
              paste0(code, "start exclude: ", paste(start[-1], collapse = " "), "\n")
      }
      
      code <- paste0(code, "\n")
      
      format_POMDP_df <-
        function(x,
          type = c("T", "R", "O"),
          digits = 7) {
          type <- match.arg(type)
          code <- ""
          
          var_cols <- seq_len(ncol(x) - 1L)
          value_col <- ncol(x)
          
          # fix indexing and convert factor to character
          for (j in var_cols) {
            if (is.numeric(x[[j]]))
              x[[j]] <- as.integer(x[[j]]) - 1L
            if (is.factor(x[[j]])) {
              x[[j]] <- as.character(x[[j]])
              x[[j]][is.na(x[[j]])] <- "*"
            }
          }
          
          # write lines
          for (i in 1:nrow(x)) {
            code <- paste0(
              code,
              type,
              ": ",
              paste(x[i, var_cols], collapse = " : "),
              " ",
              format_fixed(x[i, value_col], digits = digits, type),
              "\n"
            )
          }
          paste0(code, "\n")
        }
      
      format_POMDP_matrix <-
        function(x,
          type = c("T", "R", "O"),
          action,
          start = NULL,
          digits = 7) {
          ### sparse?
          if (inherits(x, "Matrix"))
            return(format_POMDP_dgc(x, type, action, start, digits))
          
          type <- match.arg(type)
          
          if (type == "R")
            code <- paste0(type, ": ", action, " : ", start, "\n")
          else
            code <- paste0(type, ": ", action, "\n")
          
          if (is.character(x) && length(x) == 1)
            code <- paste0(code, x, "\n\n")
          else
            code <-
            paste0(code,
              format_fixed(x,
                digits,
                type),
              "\n\n")
          
          code
        }
      
      format_POMDP_dgc <-
        function(x,
          type = c("T", "R", "O"),
          action,
          start = NULL,
          digits = 7) {
          type <- match.arg(type)
         
          x <- as(x, "TsparseMatrix")
          
          # empty matrix
          if (nnzero(x) == 0L)
            return()
          
          if (type == "R")
            prefix <- paste(action, start, sep = " : ")
          else
            prefix <- action
          
          
          code <- paste0(type,
            ": ",
            prefix,
            " : ",
            x@i,
            " : ",
            x@j,
            " ",
            sprintf(paste0("%.", digits, "f"), x@x))
          
          code <- paste0(paste0(code, collapse = "\n"), "\n\n")
          return (code)
        }
      
      ### Transition Probabilities
      if (is.data.frame(transition_prob)) {
        code <-
          paste0(code, format_POMDP_df(transition_prob, "T", digits))
      } else{
        # list of matrices
        ## if the observation probabilities are given in the form of action dependent matrices
        if (!identical(names(transition_prob), '*') &&
            !setequal(names(transition_prob), actions))
          stop("the names of given transition probability matrices do not match the actions!")
        
        # writing the transition probability matrices
        for (a in names(transition_prob)) {
          code <-
            paste0(code,
              format_POMDP_matrix(transition_prob[[a]], "T", a, digits = digits))
        }
      }
      ### Observation Probabilities
      if (is.data.frame(observation_prob)) {
        code <-
          paste0(code, format_POMDP_df(observation_prob, "O", digits))
      } else{
        if (!identical(names(observation_prob), '*') &&
            !setequal(names(observation_prob), actions))
          stop("the names of given observation probability matrices do not match the actions!")
        
        # writing the observation probability matrices
        for (a in names(observation_prob)) {
          code <-
            paste0(code,
              format_POMDP_matrix(observation_prob[[a]], "O", a, digits = digits))
        }
      }
      
      ### Rewards/Costs
      if (is.data.frame(reward)) {
        code <-
          paste0(code, format_POMDP_df(reward, "R", digits))
      } else {
        if (!identical(names(reward), '*') &&
            !setequal(names(reward), actions))
          stop("names of the rewards list do not match the actions!")
        
        for (a in names(reward)) {
          if (!identical(names(reward[[a]]), '*') &&
              !setequal(names(reward[[a]]), states))
            stop("names of the second level of the rewards list do not match the states!")
          
          for (s in names(reward[[a]])) {
            code <-
              paste0(code,
                format_POMDP_matrix(reward[[a]][[s]], "R", a, s, digits = digits))
          }
        }
      }
      ### saving the POMDP file
      cat(code, file = file)
    })
}




#' @rdname write_POMDP
#' @param parse_matrices character; parse model matrices can be either `"no"`, `"dense"`, or `"sparse"`.
#'  Use `"no"` to disable the experimental R parser.
#'  Solvers still work, but helpers for simulation are not available.
#' @export
read_POMDP <- function(file, parse_matrices = "sparse") {
  parse_matrices <-
    match.arg(parse_matrices, c("no", "dense", "sparse"))
  sparse <- parse_matrices == "sparse"
  
  problem <- readLines(file)
  
  get_vals <- function(var) {
    label <- paste0("^", var, "\\s*:\\s*")
    ind <- grep(label, problem)
    if (length(ind) == 0L)
      return(NULL)
    
    if (length(ind) > 1L)
      stop("Multiple definitions for ", var)
    
    vals <-
      strsplit(trimws(sub(label, "", problem[ind])), "\\s+")[[1]]
    
    # the data may be in the next line
    if (length(vals) == 0L)
      vals <- strsplit(problem[[ind + 1]], "\\s+")[[1]]
    
    # numbers?
    vals <- type.convert(vals, as.is = TRUE)
    vals
  }
  
  discount <- get_vals("discount")
  
  start <-  get_vals("start")
  if (is.numeric(start))
    start <-  round_stochastic(start)
  if (is.null(start))
    start <- "uniform"
  
  states <- get_vals("states")
  if (is.integer(states) && length(states == 1L))
    states <- paste0("s", seq_len(states))
  observations <- get_vals("observations")
  if (is.integer(observations) && length(observations == 1L))
    observations <- paste0("o", seq_len(observations))
  actions <- get_vals("actions")
  if (is.integer(actions) && length(actions == 1L))
    actions <- paste0("a", seq_len(actions))
  
  transition_prob <- NULL
  observation_prob <- NULL
  reward <- NULL
  
  if (parse_matrices != "no") {
    try(transition_prob <-
        parse_POMDP_matrix(problem,
          field = "T",
          actions,
          states,
          observations,
          sparse = sparse))
    try(observation_prob <-
        parse_POMDP_matrix(problem,
          field = "O",
          actions,
          states,
          observations,
          sparse = sparse))
    try(reward <-
        parse_POMDP_matrix(problem,
          field = "R",
          actions,
          states,
          observations,
          sparse = sparse))
  }
  
  x <- list(
    name = file,
    states = states,
    observations = observations,
    actions = actions,
    start = start,
    discount = discount,
    transition_prob = transition_prob,
    observation_prob = observation_prob,
    reward = reward,
    problem = structure(problem, class = "text")
  )
  
  class(x) <- c("POMDP", "list")
  x <- check_and_fix_MDP(x)
  x
}

# helpers to parse transition matrices, observation matrices and reward matrices

# Convert states/observations to labels or 1-based indices
idx <- function(x) {
  x <- type.convert(x, as.is = TRUE)
  if (is.integer(x))
    x <- x + 1L
  x
}

# parse transition and observation matrix data
parse_POMDP_matrix <-
  function(problem,
    field = c("T", "O", "R"),
    actions,
    states,
    observations,
    sparse = TRUE) {
    field <- match.arg(field)
    
    if (field == "R")
      return(.parse_POMDP_reward(problem, actions, states, observations, sparse))
    
    rows <- states
    
    if (field == "T")
      cols <- states
    
    if (field == "O")
      cols <- observations
    
    if (!sparse)
      trans <- lapply(
        actions,
        FUN = function(a)
          matrix(
            0,
            nrow = length(rows),
            ncol = length(cols),
            dimnames = (list(rows, cols))
          )
      )
    else
      trans <- lapply(
        actions,
        FUN = function(a)
        {
          m <- spMatrix(length(rows), length(cols))
          dimnames(m) <- list(rows, cols)
          m
        }
      )
    
    names(trans) <- actions
    # trans
    
    read_val_line <- function(i)
      as.numeric(strsplit(trimws(problem[i]), "\\s+")[[1]])
    
    label <- paste0("^", field, "\\s*:\\s*")
    ind <- grep(label, problem)
    
    for (i in ind) {
      vals <- sub("\\s*#.*$", "", problem[i])
      vals <- trimws(sub(label, "", vals))
      vals <- strsplit(vals, "\\s*:\\s*")[[1]]
      if (!is.na(vals[3])) {
        val4 <- strsplit(vals[3], "\\s+")[[1]]
        if (length(val4) == 2L) {
          vals[3:4] <- val4
        }
      }
      
      # For debugging
      # cat("Processing", field, "at line", i, "-", paste(vals, collapse = ";"), "\n")
      
      start <- idx(vals[2])
      end <- idx(vals[3])
      
      acts <- vals[1]
      if (acts == "*")
        acts <- actions
      for (action in acts) {
        # Case: T: <action> : <start-state> : <end-state> %f
        if (length(vals) == 4L) {
          if (start == '*' && end == '*')
            trans[[action]][] <- as.numeric(vals[4])
          else if (start == '*')
            trans[[action]][, end] <- as.numeric(vals[4])
          else if (end == '*')
            trans[[action]][start,] <- as.numeric(vals[4])
          else
            trans[[action]][start, end] <- as.numeric(vals[4])
        }
        
        # Case: T: <action> : <start-state> : <end-state>
        # %f
        if (length(vals) == 3L) {
          if (start == '*' && end == '*')
            trans[[action]][] <- read_val_line(i + 1L)
          else if (start == '*')
            trans[[action]][, end] <- read_val_line(i + 1L)
          else if (end == '*')
            trans[[action]][start,] <- read_val_line(i + 1L)
          else
            trans[[action]][start, end] <- read_val_line(i + 1L)
        }
        
        # Case: T: <action> : <start-state>
        #       %f %f ... %f
        if (length(vals) == 2L) {
          if (start == '*')
            for (k in seq_along(rows))
              trans[[action]][k,] <- read_val_line(i + 1L)
          else
            trans[[action]][start, ] <- read_val_line(i + 1L)
        }
        
        # Case: T: <action>
        # %f %f ... %f
        # %f %f ... %f
        #...
        # %f %f ... %f
        if (length(vals) == 1L) {
          special <- pmatch(problem[i + 1], c("identity", "uniform"))
          if (is.na(special)) {
            for (j in seq_along(rows))
              trans[[action]][j,] <- read_val_line(i + j)
          } else if (special == 1) {
            trans[[action]][] <- 0
            diag(trans[[action]]) <- 1
          } else if (special == 2) {
            trans[[action]][] <- 1 / length(cols)
          }
        }
      }
    }
    
    if (sparse)
      trans <- lapply(trans, .sparsify)
    
    trans
  }

.parse_POMDP_reward <-
  function(problem, actions, states, obs, sparse = TRUE) {
    if (!sparse)
      matrix_list <- lapply(
        actions,
        FUN = function(a) {
          l <- lapply(
            states,
            FUN = function(s)
              matrix(
                0,
                nrow = length(states),
                ncol = length(obs),
                dimnames = (list(states, obs))
              )
          )
          names(l) <- states
          l
        }
      )
    else
      matrix_list <- lapply(
        actions,
        FUN = function(a) {
          l <- lapply(
            states,
            FUN = function(s) {
              m <- spMatrix(length(states), length(obs))
              dimnames(m) <- list(states, obs)
              m
            }
          )
          names(l) <- states
          l
        }
      )
    
    names(matrix_list) <- actions
    
    #matrix_list
    
    read_val_line <- function(i)
      as.numeric(strsplit(trimws(problem[i]), "\\s+")[[1]])
    
    field <- "R"
    label <- paste0("^", field, "\\s*:\\s*")
    ind <- grep(label, problem)
    #problem[ind]
    
    for (i in ind) {
      vals <- sub("\\s*#.*$", "", problem[i])
      vals <- trimws(sub(label, "", vals))
      vals <- strsplit(vals, "\\s*:\\s*")[[1]]
      if (!is.na(vals[4])) {
        val4 <- strsplit(vals[4], "\\s+")[[1]]
        if (length(val4) == 2L) {
          vals[4:5] <- val4
        }
      }
      
      # For debugging
      #cat("Processing (", i, ")", paste(vals, collapse = ";"), "\n")
      
      end <- idx(vals[3])
      obs <- idx(vals[4])
      
      starts <- idx(vals[2])
      acts <- vals[1]
      if (acts == "*")
        acts <- actions
      
      if (starts == "*")
        starts <- states
      
      for (action in acts) {
        for (start in starts) {
          # Case: R: <action> : <start-state> : <end-state> : <observation> %f
          if (length(vals) == 5L) {
            if (end == '*' && obs == '*')
              matrix_list[[action]][[start]][] <-
                as.numeric(vals[5])
            else if (end == '*')
              matrix_list[[action]][[start]][, obs] <-
                as.numeric(vals[5])
            else if (obs == '*')
              matrix_list[[action]][[start]][end,] <-
                as.numeric(vals[5])
            else
              matrix_list[[action]][[start]][end, obs] <-
                as.numeric(vals[5])
          }
          
          # Case: R: <action> : <start-state> : <end-state> : <observation>
          # %f
          if (length(vals) == 4L) {
            if (end == '*' && obs == '*')
              matrix_list[[action]][[start]][] <-
                read_val_line(i + 1L)
            else if (end == '*')
              matrix_list[[action]][[start]][, obs] <-
                read_val_line(i + 1L)
            else if (obs == '*')
              matrix_list[[action]][[start]][end,] <-
                read_val_line(i + 1L)
            else
              matrix_list[[action]][[start]][end, obs] <-
                read_val_line(i + 1L)
          }
          
          # Case: R: <action> : <start-state> : <end-state>
          #       %f %f ... %f
          if (length(vals) == 3L) {
            if (end == '*')
              for (k in seq_along(states))
                matrix_list[[action]][[start]][k,] <-
                  read_val_line(i + 1L)
            else
              matrix_list[[action]][[start]][end, ] <-
                read_val_line(i + 1L)
          }
          
          # Case: R: <action> : <start-state>
          # %f %f ... %f
          # %f %f ... %f
          #...
          # %f %f ... %f
          if (length(vals) == 2L) {
            for (j in seq_along(states))
              matrix_list[[action]][[start]][j,] <-
                read_val_line(i + j)
          }
        }
      }
    }
    
    if (sparse)
      matrix_list <- lapply(matrix_list, lapply, .sparsify)
    
    matrix_list
  }