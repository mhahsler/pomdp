# Convert specification and read and write auxiliary files used by pomdp-solve

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


# helpers to read/write pomdp-solve files

# alpha file is a matrix with # of states columns
.write_alpha_file <- function(file_prefix, alpha, digits = 7) {
  filename <- paste0(file_prefix, '_terminal_values.alpha')
  if (!is.matrix(alpha))
    alpha <- rbind(alpha)
  
  # we don't care about the action so we always use "0"
  for (i in seq(nrow(alpha)))
    cat(
      "0",
      paste0(.format_number_fixed(alpha[i, ], digits = digits), collapse = " "),
      "",
      file = filename,
      sep = "\n",
      append = i > 1
    )
  filename
}

.get_alpha_file <- function(file_prefix, model, number = "") {
  filename <- paste0(file_prefix, '-0.alpha', number)
  ## importing alpha file
  alpha <- readLines(filename)
  alpha <- alpha[seq(2, length(alpha), 3)]
  alpha <-
    do.call(rbind, lapply(alpha, function(a)
      as.numeric(strsplit(a, " ")[[1]])))
  colnames(alpha) <- model$states
  alpha
}

## importing pg file
.get_pg_file <- function(file_prefix, model, number = "") {
  filename <- paste0(file_prefix, '-0.pg', number)
  pg <- read.table(
    filename,
    header = FALSE,
    sep = "",
    colClasses = "numeric",
    na.strings = c("-", "X")
  )
  pg <- pg + 1 #index has to start from 1 not 0
  
  # renaming the columns and actions
  colnames(pg) <-
    c("node", "action", as.character(model$observations))
  pg[, 2] <- factor(pg[, 2], levels = seq(length(model$actions)), labels = model$actions)
  pg
}

# importing belief file (used belief points) if it exists (only grid method)
.get_belief_file <- function(file_prefix, model) {
  filename <- paste0(file_prefix, '-0.belief')
  if (!file.exists(filename))
    return(NULL)
  
  belief <- as.matrix(read.table(filename))
  colnames(belief) <- as.character(model$states)
  belief
}

# write belief file to specify which belief points the grid method should use.
.write_grid_file <- function(file_prefix, belief, digits = 7) {
  filename <- paste0(file_prefix, '.grid')
  if (!is.matrix(belief))
    belief <- rbind(belief)
  
  
  cat(.format_number_fixed(belief, digits), file = filename)
  filename
}
