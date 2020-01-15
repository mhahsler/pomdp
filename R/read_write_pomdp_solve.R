# Convert sepcifications and read and write auxiliary files used by pomdp-solve

# translate belief specifications into numeric belief vectors
.translate_belief <- function(belief = NULL, model) {
  ## producing the starting belief vector
  
  states <- as.character(model$model$states)
  
  if(is.null(belief)) belief <- "uniform"
 
  if(is.matrix(belief)) {
    if(ncol(belief) != length(states)) stop("Number of column is not the number if states.")
    colnames(belief) <- states
    return(belief)
  }
   
  
  # start: 0.3 0.1 0.0 0.2 0.5
  if(is.numeric(belief) &&
      length(belief) == length(states) && 
      round(sum(belief), 3) == 1) {
    names(belief) <- states
    return(belief) 
  }
  
  # start: uniform
  if(is.character(belief) &&
      length(belief) == 1 && 
      belief[1] == "uniform") {
    belief <- rep(1/length(states), times = length(states))
    names(belief) <- states
    return(belief) 
  }
  
  
  # general checks for state IDs
  if(is.numeric(belief)) {
    belief <- as.integer(belief)
    if(any(abs(belief) <1) || any(abs(belief) > length(states)))
      stop("Illegal belief format.\n", belief,
        "\nState IDs need to be in [1, # of states].")
  }
  
  # general checks for state names
  else if(is.character(belief)) {
    if(any(is.na(match(belief, c(as.character(states), "-")))))
      stop("Illegal belief format.\n", belief,
        "\nUnrecognized state name.")
  
  } else stop("Illegal belief format.")
  
  #start: first-state
  #start: 5
  #start include: first-state third state
  #start include: 1 3
  if((is.numeric(belief) && all(belief > 0)) ||
      (is.character(belief) && belief[1] != "-")) {
    if(length(belief) > length(states)) 
      stop("Illegal belief format.\n", belief,
        "\nToo many states specified.")
    belief_ <- rep(0, times = length(states))
    names(belief_) <- states
    belief_[belief] <- 1/length(belief) 
    return(belief_)
  }
  
  #start exclude: 1 3
  if(is.numeric(belief) && any(belief < 0)) {
    belief_ <- rep(1, times = length(states))
    if(length(belief) >= length(states)) 
      stop("Illegal belief format.\n", belief,
        "\nToo many states specified.")
    names(belief_) <- states
    belief_[-belief] <- 0
    belief_ <- belief_/sum(belief_)
    return(belief_)
  }
  
  #start exclude: fifth-state seventh-state
  if(is.character(belief) && belief[1] == "-") {
    belief <- belief[-1]
    belief_ <- rep(1, times = length(states))
    names(belief_) <- states
    belief_[belief] <- 0
    belief_ <- belief_/sum(belief_)
    return(belief_)
  }
  
  stop("Illegal belief format.\n", belief)
}

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

.translate_probabilities <- function(model, 
  field = "transition_prob", from = "states", to = "states") {
  
  actions <- model$model$actions
  prob <-  model$model[[field]]
  
  if(is.data.frame(prob)) {
    prob <- sapply(actions, function(a) {
      .df2matrix(model, 
        prob[(prob$action == a | prob$action == "*"), 2:4],
        from = from, to = to)
    }, simplify = FALSE, USE.NAMES = TRUE)
    
  } else if(is.list(prob)) {
    from <- model$model[[from]]
    to <- model$model[[to]]
    
    prob <- lapply(prob, FUN = function(tr) {
      if(is.character(tr)) {
        tr <- switch(tr, 
          identity = diag(1, nrow = length(from), ncol = length(to)),
          uniform = matrix(1/length(to), 
            nrow = length(from), ncol = length(to)))
      }
      
      if(!is.matrix(tr)) stop("Probabilities cannot be converted to matrix.")
      dimnames(tr) <- list(from, to)
      
      tr
    })
  } else stop("Unknown transition/observation matrix format.")
  prob 
}

.translate_reward <- function(model) {
  actions <- as.character(model$model$actions)
  states <- as.character(model$model$states)
  reward <-  model$model$reward
  
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


# helpers to read/write pomdp-solve files

# alpha file is a matrix with # of states columns
.write_alpha_file <- function(file_prefix, alpha) {
  filename <- paste0(file_prefix, '_terminal_values.alpha')
  if(!is.matrix(alpha)) alpha <- rbind(alpha)
 
  # we don't care about the action so we always use "0" 
  for(i in seq(nrow(alpha))) 
    cat("0", paste0(alpha[i, ], collapse = " "), "", 
      file = filename, sep = "\n", append = i>1)
  filename
}  

.get_alpha_file <- function(file_prefix, model, number = "") {  
  filename <- paste0(file_prefix, '-0.alpha',number)
  ## importing alpha file
  alpha <- readLines(filename)
  alpha <- alpha[seq(2, length(alpha), 3)]
  alpha <- do.call(rbind, lapply(alpha, function(a) as.numeric(strsplit(a, " ")[[1]])))
  colnames(alpha) <- model$model$states
  alpha
}

## importing pg file
.get_pg_file <- function(file_prefix, model, number="") {
  filename <- paste0(file_prefix,'-0.pg', number)
  pg <- read.table(filename, header = FALSE, sep = "", 
    colClasses = "numeric", na.strings = c("-", "X"))
  pg <- pg + 1 #index has to start from 1 not 0
  
  ### FIXME: I am not sure we need this now
  #if (dim(pg)[2]==1 ) {
  #  pg <- t(pg)
  #}
  
  # renaming the columns and actions
  colnames(pg) <- c("node", "action", as.character(model$model$observations))
  pg[,2] <- model$model$actions[pg[,2]]
  pg
}
  
# importing belief file (used belief points) if it exists
.get_belief_file <- function(file_prefix, model) {
  filename <- paste0(file_prefix,'-0.belief')
  if(!file.exists(filename)) return(NULL)
  
  belief <- as.matrix(read.table(filename)) 
  colnames(belief) <- as.character(model$model$states)
  belief
} 
