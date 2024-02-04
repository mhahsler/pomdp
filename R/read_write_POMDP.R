#' Read and write a POMDP Model to a File in POMDP Format
#'
#' Reads and write a POMDP file suitable for the `pomdp-solve` program.
#'
#' [POMDP] objects read from a POMDP file have an extra element called `problem` which contains the original
#' POMDP specification. **The original specification is directly used by external solvers.** In addition, the file
#' is parsed using an experimental POMDP file parser. The parsed information can be used with auxiliary functions
#' in this package that use fields like the transition matrix, the observation matrix and the reward structure.
#'
#' The range of useful rewards is restricted by the solver. Here the values are restricted to the range
#' `[-1e10, 1e10]`. 
#' Unavailable actions have a reward of `-Inf` which is translated to -2 times the maximum
#' absolute reward value used in the model.
#'
#' **Notes:**
#' The parser for POMDP files is experimental. Please report
#' problems here: \url{https://github.com/mhahsler/pomdp/issues}.
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
  
  # we write the problem field if we have it
  #if(!is.null(x$problem)) {
  #  cat(x$problem, file = file)
  #  return()
  #}
  
  ### solver restrictions. POMDP defines a float for the reward 
  mr <- .max_abs_reward(x)
  reward_max <- 1e10
  if (mr > reward_max)
    stop(
      "Reward values supported by the solver need to be in [",
      -reward_max,
      ", ",+reward_max,
      "]."
    )
  
  reward_neg_Inf <- -2*mr
  #reward_neg_Inf <- -1e15
 
  pomdp_solve_OK_chars <- "[^A-Za-z0-9_-]"
  
  x <- check_and_fix_MDP(x)
  
  if (is.character(file)) {
    file <- file(file, "w")
    on.exit(close(file))
  }
  
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
         
         # state names cannot contain special characters!
         if (is.character(states) &&
             any(grepl(pomdp_solve_OK_chars, states)))
           stop(
             "Some state labels use caracters unsupported by the solver! Only use ",
             pomdp_solve_OK_chars
           )
         if (is.character(actions) &&
             any(grepl(pomdp_solve_OK_chars, actions)))
           stop(
             "Some action labels use caracters unsupported by the solver! Only use ",
             pomdp_solve_OK_chars
           )
         if (is.character(observations) &&
             any(grepl(pomdp_solve_OK_chars, observations)))
           stop(
             "Some observation labels use caracters unsupported by the solver! Only use ",
             pomdp_solve_OK_chars
           )
         
         ### POMDP file
         preamble <-  paste0(
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
               preamble_start <-
                 paste0("start: ",
                        .format_number_fixed(round_stochastic(start, digits), digits, "start"),
                        "\n")
             } else {
               ## this should be indices (pomdp_solve starts with 0)
               start_ids <- as.integer(abs(start)) - 1L
               if (all(start < 0))
                 preamble_start <-
                   paste0("start include: ",
                          paste(start_ids, collapse = " "),
                          "\n")
               if (all(start > 0))
                 preamble_start <-
                   paste0("start exclude: ",
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
               preamble_start <-
                 paste0("start: ", paste(start, collapse = " "), "\n")
               
             } else if (start[1] != "-")
               preamble_start <-
                 paste0("start include: ",
                        paste(start, collapse = " "),
                        "\n")
             else
               preamble_start <-
                 paste0("start exclude: ",
                        paste(start[-1], collapse = " "),
                        "\n")
         }
         
         cat(preamble, preamble_start, "\n", file = file)
         
         ### write preamble
         
         format_POMDP_df <-
           function(x,
                    file,
                    type = c("T", "R", "O"),
                    digits = 7) {
             type <- match.arg(type)
             
             if (type == 'R')
               cat(
                 paste0(
                   type,
                   ": ",
                   .to_0_idx(x[[1L]]),
                   " : ",
                   .to_0_idx(x[[2L]]),
                   " : ",
                   .to_0_idx(x[[3L]]),
                   " : ",
                   .to_0_idx(x[[4L]]),
                   " ",
                   sprintf(paste0("%.", digits, "f"), x[[5L]]),
                   collapse = "\n"
                 ),
                 file = file,
                 append = TRUE
               )
             
             else
               cat(
                 paste0(
                   type,
                   ": ",
                   .to_0_idx(x[[1L]]),
                   " : ",
                   .to_0_idx(x[[2L]]),
                   " : ",
                   .to_0_idx(x[[3L]]),
                   " ",
                   sprintf(paste0("%.", digits, "f"), x[[4L]]),
                   collapse = "\n"
                 ),
                 file = file,
                 append = TRUE
               )
             cat("\n", file = file)
             
           }
         
         format_POMDP_matrix <-
           function(x,
                    file,
                    type = c("T", "R", "O"),
                    action,
                    start = NULL,
                    digits = 7) {
             type <- match.arg(type)
             
             ### sparse?
             if (inherits(x, "Matrix"))
               return(format_POMDP_dgc(x, file, type, action, start, digits))
             
             if (type == "R")
               code <- paste0(type, ": ", action, " : ", start, "\n")
             else
               code <- paste0(type, ": ", action, "\n")
             
             if (is.character(x) && length(x) == 1)
               code <- paste0(code, x, "\n")
             else
               code <-
                 paste0(code,
                        .format_number_fixed(x,
                                             digits,
                                             type),
                        "\n")
             
             cat(code, file = file, append = TRUE)
             cat("\n", file = file)
           }
         
         format_POMDP_dgc <-
           function(x,
                    file,
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
                            #rownames(x)[x@i + 1L],
                            " : ",
                            x@j,
                            #colnames(x)[x@j + 1],
                            " ",
                            sprintf(paste0("%.", digits, "f"), x@x))
             
             code <- paste0(code, collapse = "\n")
             
             cat(code, file = file, append = TRUE)
             cat("\n\n", file = file)
           }
         
         ### Transition Probabilities
         if (is.data.frame(transition_prob))
           format_POMDP_df(transition_prob, file, "T", digits)
         else{
           # list of matrices
           ## if the observation probabilities are given in the form of action dependent matrices
           if (!identical(names(transition_prob), '*') &&
               !setequal(names(transition_prob), actions))
             stop("the names of given transition probability matrices do not match the actions!")
           
           # writing the transition probability matrices
           for (a in names(transition_prob))
             format_POMDP_matrix(transition_prob[[a]], file, "T", a, digits = digits)
         }
         
         ### Observation Probabilitieus
         if (is.data.frame(observation_prob))
           format_POMDP_df(observation_prob, file, "O", digits)
         else{
           if (!identical(names(observation_prob), '*') &&
               !setequal(names(observation_prob), actions))
             stop("the names of given observation probability matrices do not match the actions!")
           
           # writing the observation probability matrices
           for (a in names(observation_prob))
             format_POMDP_matrix(observation_prob[[a]], file, "O", a, digits = digits)
         }
         
         ### TODO
         ### Rewards/Costs
         #   1. all reward calculations use float (3.4E-38 to 3.4E+38), rewards need to be small
         #   2. reward cannot be Inf/-Inf.
         #   3. translate -Inf to a large negative number
         
         if (is.data.frame(reward)) {
           if (any((reward$value < -reward_max |
                    reward$value > reward_max) &
                   reward$value != -Inf))
             stop(
               "Reward values supported by the solver need to be in [",
               -reward_max,
               ", ",+reward_max,
               "]."
             )
           reward$value[reward$value == -Inf] <- reward_neg_Inf
           
           format_POMDP_df(reward, file, "R", digits)
         }
         else {
           if (!identical(names(reward), '*') &&
               !setequal(names(reward), actions))
             stop("names of the rewards list do not match the actions!")
           
           for (a in names(reward)) {
             if (!identical(names(reward[[a]]), '*') &&
                 !setequal(names(reward[[a]]), states))
               stop("names of the second level of the rewards list do not match the states!")
             
             for (s in names(reward[[a]])) {
               reward_matrix <- reward[[a]][[s]]
               if (any((
                 reward_matrix < -reward_max |
                 reward_matrix > reward_max
               ) &
               reward_matrix != -Inf
               ))
                 stop(
                   "Reward values supported by the solver need to be in [",
                   -reward_max,
                   ",",+reward_max,
                   "]"
                 )
               reward_matrix[reward_matrix == -Inf] <- reward_neg_Inf
               
               format_POMDP_matrix(reward_matrix, file, "R", a, s, digits = digits)
             }
           }
         }
       })
}


#' @rdname write_POMDP
#' @param parse logical; try to parse the model matrices.
#'  Solvers still work with unparsed matrices, but helpers for simulation are not available.
#' @param normalize logical; should the description be normalized for faster access (see [normalize_POMDP()])?
#' @export
read_POMDP <- function(file,
                       parse = TRUE,
                       normalize = TRUE) {
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
  
  ### expand states, actions and observations
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
  
  if (parse) {
    transition_prob <- NULL
    try(transition_prob <-
          parse_POMDP_df(problem,
                         field = "T"))
    if (is.null(transition_prob))
      try(transition_prob <-
            parse_POMDP_matrix(problem,
                               field = "T",
                               actions,
                               states,
                               observations,
                               sparse = TRUE))
    
    observation_prob <- NULL
    try(observation_prob <-
          parse_POMDP_df(problem,
                         field = "O"))
    if (is.null(observation_prob))
      try(observation_prob <-
            parse_POMDP_matrix(problem,
                               field = "O",
                               actions,
                               states,
                               observations,
                               sparse = TRUE))
    
    reward <- NULL
    try(reward <-
          parse_POMDP_df(problem,
                         field = "R"))
    if (is.null(reward))
      try(reward <-
            parse_POMDP_matrix(problem,
                               field = "R",
                               actions,
                               states,
                               observations,
                               sparse = TRUE))
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
  
  if (parse && normalize)
    x <- normalize_POMDP(x)
  
  x
}

# helpers to parse transition matrices, observation matrices and reward matrices

# sprintf wrapper for vectors and matrices
.format_number_fixed <- function(x, digits = 7, debug = "unknown") {
  if (is.null(x))
    stop("missing field ", debug)
  
  if (is.vector(x)) {
    if (!is.numeric(x))
      stop("Write_POMDP expects numbers, but got: ", dQuote(paste(x, collapse = ", ")))
    paste(sprintf(paste0("%.", digits, "f"), x), collapse = " ")
  } else if (is.matrix(x)) {
    paste(apply(
      x,
      MARGIN = 1,
      .format_number_fixed,
      digits = digits
    ),
    collapse = "\n")
  } else
    stop("formating not implemented for ", class(x), " in field ", debug)
}


# The POMDP file format uses 0-based indices.
# Convert states/observations to labels or 1-based indices. Also converts asterisks to NA
.from_0_idx <- function(x) {
  x[x == "*"] <- NA
  x <- type.convert(x, as.is = TRUE)
  if (is.integer(x))
    x <- x + 1L
  x
}

.to_0_idx <- function(x) {
  x <- as.character(as.integer(x) - 1L)
  x[is.na(x)] <- '*'
  x
}

parse_POMDP_df <- function(problem,
                           field = c("T", "O", "R"),
                           actions = NULL,
                           states = NULL,
                           observations = NULL,
                           sparse = TRUE) {
  field <- match.arg(field)
  
  if (field == "R") {
    field_len <- 6
  } else {
    field_len <- 5
  }
  
  label <- paste0("^", field, "\\s*:\\s*")
  ind <- grep(label, problem)
  
  field_values <- problem[ind]
  field_values <- sub("\\s*#.*$", "", field_values)
  field_values <- trimws(field_values)
  field_values <- strsplit(field_values, "\\s*:\\s*|\\s+")
  
  if (any(sapply(field_values, length) != field_len))
    return (NULL)
  
  dat <- switch(
    field,
    R = R_(
      action = .from_0_idx(sapply(field_values, "[", 2L)),
      start.state = .from_0_idx(sapply(field_values, "[", 3L)),
      end.state = .from_0_idx(sapply(field_values, "[", 4L)),
      observation = .from_0_idx(sapply(field_values, "[", 5L)),
      value = as.numeric(sapply(field_values, "[", 6L))
    ),
    
    T =  T_(
      action = .from_0_idx(sapply(field_values, "[", 2L)),
      start.state = .from_0_idx(sapply(field_values, "[", 3L)),
      end.state = .from_0_idx(sapply(field_values, "[", 4L)),
      probability = as.numeric(sapply(field_values, "[", 5L))
    ),
    
    O = O_(
      action = .from_0_idx(sapply(field_values, "[", 2L)),
      end.state = .from_0_idx(sapply(field_values, "[", 3L)),
      observation = .from_0_idx(sapply(field_values, "[", 4L)),
      probability = as.numeric(sapply(field_values, "[", 5L))
    )
  )
  
  dat
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
      # cat("Parsing line ", i, "\n")
      
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
      
      start <- .from_0_idx(vals[2])
      end <- .from_0_idx(vals[3])
      
      acts <- .from_0_idx(vals[1])
      if (is.na(acts))
        acts <- actions
      for (action in acts) {
        if (length(vals) == 4L) {
          if (is.na(start) && is.na(end))
            trans[[action]][] <- as.numeric(vals[4])
          else if (is.na(start))
            trans[[action]][, end] <- as.numeric(vals[4])
          else if (is.na(end))
            trans[[action]][start,] <- as.numeric(vals[4])
          else
            trans[[action]][start, end] <- as.numeric(vals[4])
        }
        
        # Case: T: <action> : <start-state> : <end-state>
        # %f
        if (length(vals) == 3L) {
          if (is.na(start) && is.na(end))
            trans[[action]][] <- read_val_line(i + 1L)
          else if (is.na(start))
            trans[[action]][, end] <- read_val_line(i + 1L)
          else if (is.na(end))
            trans[[action]][start,] <- read_val_line(i + 1L)
          else
            trans[[action]][start, end] <- read_val_line(i + 1L)
        }
        
        # Case: T: <action> : <start-state>
        #       %f %f ... %f
        if (length(vals) == 2L) {
          if (is.na(start))
            for (k in seq_along(rows))
              trans[[action]][k,] <- read_val_line(i + 1L)
          else
            trans[[action]][start, ] <- read_val_line(i + 1L)
        }
        
        # Case: T: <action>
        # %f %f ... %f
        # %f %f ... %f
        # %f %f ... %f
        #...
        # %f %f ... %f
        # or
        # [uniform/identity]
        if (length(vals) == 1L) {
          special <- pmatch(problem[i + 1], c("identity", "uniform"))
          if (is.na(special)) {
            for (j in seq_along(rows))
              trans[[action]][j,] <- read_val_line(i + j)
          } else {
            trans[[action]] <- c("identity", "uniform")[special]
          }
        }
      }
    }
    
    if (sparse)
      trans <- lapply(trans, .sparsify)
    
    trans
  }


.parse_POMDP_reward <-
  function(problem, actions, states, observations, sparse = TRUE) {
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
                ncol = length(observations),
                dimnames = (list(states, observations))
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
              m <- spMatrix(length(states), length(observations))
              dimnames(m) <- list(states, observations)
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
      
      ### this also translates *
      acts <- .from_0_idx(vals[1])
      starts <- .from_0_idx(vals[2])
      end <- .from_0_idx(vals[3])
      obs <- .from_0_idx(vals[4])
      if (is.na(acts))
        acts <- actions
      if (is.na(starts))
        starts <- states
      if (is.na(obs))
        obs <- observations
      if (is.na(end))
        end <- states
      
      for (action in acts) {
        for (start in starts) {
          # Case: R: <action> : <start-state> : <end-state> : <observation> %f
          if (length(vals) == 5L) {
              matrix_list[[action]][[start]][end, obs] <-
                as.numeric(vals[5])
          }
          
          # Case: R: <action> : <start-state> : <end-state> : <observation>
          # %f
          if (length(vals) == 4L) {
              matrix_list[[action]][[start]][end, obs] <-
                read_val_line(i + 1L)
          }
          
          # Case: R: <action> : <start-state> : <end-state>
          #       %f %f ... %f
          if (length(vals) == 3L) {
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

# translate belief specifications into numeric belief vectors
.translate_belief <- function(belief = NULL, model) {
  ## producing the starting belief vector
  
  states <- as.character(model$states)
  
  if (is.null(belief))
    belief <- model$start
  if (is.null(belief))
    belief <- "uniform"
  
  if (any(is.na(belief)))
    return(belief)
  
  if (is.matrix(belief)) {
    if (ncol(belief) != length(states))
      stop("Number of column is not the number of states.")
    colnames(belief) <- states
    return(belief)
  }
  
  # start: 0.3 0.1 0.0 0.2 0.5
  if (is.numeric(belief) &&
      length(belief) == length(states) &&
      round(sum(belief), 3) == 1) {
    if(!is.null(names(belief)) && !all(names(belief) == states))
      names(belief) <- states
    return(belief)
  }
  
  # start: uniform
  if (is.character(belief) &&
      length(belief) == 1 &&
      belief[1] == "uniform") {
    belief <- rep(1 / length(states), times = length(states))
    names(belief) <- states
    return(belief)
  }
  
  
  # general checks for state IDs
  if (is.numeric(belief)) {
    belief <- as.integer(belief)
    if (any(abs(belief) < 1) || any(abs(belief) > length(states)))
      stop("Illegal belief format.\n",
           belief,
           "\nState IDs need to be in [1, # of states].")
  }
  
  # general checks for state names
  else if (is.character(belief)) {
    if (any(is.na(match(belief, c(
      as.character(states), "-"
    )))))
      stop("Illegal belief format.\n",
           belief,
           "\nUnrecognized state name.")
    
  } else
    stop("Illegal belief format.")
  
  #start: first-state
  #start: 5
  #start include: first-state third state
  #start include: 1 3
  if ((is.numeric(belief) && all(belief > 0)) ||
      (is.character(belief) && belief[1] != "-")) {
    if (length(belief) > length(states))
      stop("Illegal belief format.\n",
           belief,
           "\nToo many states specified.")
    belief_ <- rep(0, times = length(states))
    names(belief_) <- states
    belief_[belief] <- 1 / length(belief)
    return(belief_)
  }
  
  #start exclude: 1 3
  if (is.numeric(belief) && any(belief < 0)) {
    belief_ <- rep(1, times = length(states))
    if (length(belief) >= length(states))
      stop("Illegal belief format.\n",
           belief,
           "\nToo many states specified.")
    names(belief_) <- states
    belief_[-belief] <- 0
    belief_ <- belief_ / sum(belief_)
    return(belief_)
  }
  
  #start exclude: fifth-state seventh-state
  if (is.character(belief) && belief[1] == "-") {
    belief <- belief[-1]
    belief_ <- rep(1, times = length(states))
    names(belief_) <- states
    belief_[belief] <- 0
    belief_ <- belief_ / sum(belief_)
    return(belief_)
  }
  
  stop("Illegal belief format.\n", belief)
}
