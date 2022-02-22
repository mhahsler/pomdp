# FIXME: we should use pomdp:::round_stochastic here!
# TODO: Improve read_POMDP to read probability matrices.

format_fixed <- function(x, digits = 7, debug = "unknown") {
  if (is.null(x))
    stop("missing field ", debug)
  
  if (is.vector(x))
    paste(sprintf(paste0("%.", digits, "f"), x), collapse = " ")
  else if (is.matrix(x))
    paste(apply(x, MARGIN = 1, format_fixed, digits = digits), collapse = "\n")
  else
    stop("formating not implemented for ", class(x), " in field ", debug)
}

#' Read and write a POMDP Model to a File in POMDP Format
#'
#' Reads and write a POMDP file suitable for the `pomdp-solve` program. _Note:_ read POMDP files are intended to be used in [solve_POMDP()] and do not support all auxiliary functions. Fields like the transition matrix, the observation matrix and the reward structure are not parsed.
#'
#' @family POMDP
#' 
#' @param x an object of class [POMDP].
#' @param digits precision for writing numbers (digits after the decimal
#' point).
#' @param file a file name.
#' @return `read_POMDP()` returns a [POMDP] object.
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @references POMDP solver website: http://www.pomdp.org
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
  with(x,
    {
      number_of_states        <- length(states)
      number_of_observations  <- length(observations)
      number_of_actions       <- length(actions)
      
      # we only support rewards and not cost
      values <- "reward"
      
      if (is.function(transition_prob))
        transition_prob <- transition_matrix(x)
      if (is.function(observation_prob))
        observation_prob <- observation_matrix(x)
      if (is.function(reward))
        reward <- reward_matrix(x)
      
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
          if (length(start) == length(states) && sum(start) == 1) {
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
      } else
        stop("Illegal specification of start.")
      
      code <- paste0(code, "\n")
      
      format_POMDP_df <-
        function(x,
          type = c("T", "R", "O"),
          digits = 7) {
          type <- match.arg(type)
          code <- ""
          
          var_cols <- seq_len(ncol(x) - 1L)
          value_col <- ncol(x)
          
          # fix indexing
          for (j in var_cols)
            if (is.numeric(x[[j]]))
              x[[j]] <- as.integer(x[[j]]) - 1L
          
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
          digits = 7) {
          code <- ""
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
      
      
      ### Transition Probabilities
      if (is.data.frame(transition_prob)) {
        check_df(transition_prob, T_, "transition_prob")
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
          code <- paste0(code, "T: ", a, "\n")
          code <-
            paste0(code,
              format_POMDP_matrix(transition_prob[[a]], "T", digits))
        }
      }
      ### Observation Probabilities
      if (is.data.frame(observation_prob)) {
        check_df(observation_prob, O_, "observation_prob")
        code <-
          paste0(code, format_POMDP_df(observation_prob, "O", digits))
      } else{
        if (!identical(names(observation_prob), '*') &&
            !setequal(names(observation_prob), actions))
          stop("the names of given observation probability matrices do not match the actions!")
      }
      # writing the observation probability matrices
      for (a in names(observation_prob)) {
        code <- paste0(code, "O: ", a, "\n")
        code <-
          paste0(code,
            format_POMDP_matrix(observation_prob[[a]], "O", digits))
      }
      
      ### Rewards/Costs
      if (is.data.frame(reward)) {
        check_df(reward, R_, "reward")
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
            code <- paste0(code, "R: ", a, ":" , s, "\n")
            code <-
              paste0(code,
                format_POMDP_matrix(reward[[a]][[s]], "R", digits))
          }
        }
      }
      ### saving the POMDP file
      cat(code, file = file)
    })
}

#' @rdname write_POMDP
#' @export
read_POMDP <- function(file) {
  problem <- readLines(file)
  
  get_vals <- function(var, number = FALSE) {
    ind <- grep(paste0("^", var, ":"), problem)
    if (length(ind) == 0)
      return(NULL)
    
    vals <-
      strsplit(trimws(problem[ind]), "\\s+")[[1]][-1]
    
    # the data may be in the next line
    if (length(vals) == 0)
      vals <- strsplit(problem[[ind + 1]], "\\s+")[[1]]
    
    # numbers?
    vals <- type.convert(vals, as.is = TRUE)
    
    # create labels if just the number is mentioned
    #if (number && length(vals) == 1 && is.numeric(vals))
    #  vals <- paste0(substr(var, 1, 1), seq(vals))
    vals
  }
  
  x <- list(
    name = file,
    states = get_vals("states", number = TRUE),
    observations = get_vals("observations", number = TRUE),
    actions = get_vals("actions", number = TRUE),
    start = get_vals("start"),
    discount = get_vals("discount"),
    problem = structure(problem, class = "text")
  )
  
  class(x) <- c("POMDP", "list")
  x <- check_and_fix_MDP(x)
  x
}
