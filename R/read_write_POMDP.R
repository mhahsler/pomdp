# FIXME: we should use pomdp:::round_stochastic here!
format_fixed <- function(x, digits = 7) {
  if(is.vector(x)) paste(sprintf(paste0("%.",digits,"f"), x), collapse = " ")
  else if(is.matrix(x)) paste(apply(x, MARGIN = 1, format_fixed, digits = digits), collapse = "\n")
  else stop("formating not implemented for ", class(x))
}

#' Read and write a POMDP Model to a File in POMDP Format
#' 
#' Reads and write a POMDP file suitable for the pomdp-solve program. Note: read POMDP files are intended to be used in solve_POMDP() and do not support all auxiliary functions. Fields like the transition matrix, the observation matrix and the reward structure are not parsed.
#' 
#' 
#' @aliases write_POMDP read_POMDP
#' @param model an object of class POMDP_model.
#' @param digits precision for writing numbers (digits after the decimal
#' point).
#' @param file a file name.
#' @return \code{read_POMDP} returns a POMDP object.
#' @author Hossein Kamalzadeh, Michael Hahsler
#' @seealso \code{\link{POMDP}}
#' @references POMDP solver website: \url{http://www.pomdp.org}
#' @keywords IO
#' @export
write_POMDP <- function(model, file, digits = 7) {
  if(!inherits(model, "POMDP")) stop("model needs to be a POMDP model use POMDP()!")
  
  model <- model$model
  
  discount    <- model$discount 
  states      <- model$states 
  number_of_states <- length(states)
  actions     <- model$actions 
  number_of_actions <- length(actions)
  observations <- model$observations 
  number_of_observations <- length(observations)
  start       <- model$start 
  transition_prob <- model$transition_prob
  observation_prob <- model$observation_prob 
  reward      <- model$reward
  max         <- model$max
  values <- ifelse(max, "reward", "cost")
  
  ### POMDP file
  code <-  paste0(
    "# POMDP File: ", model$name, "\n",
    "# Produced with R package pomdp\n",
    "\n",
    "discount: ", format(discount, digits = digits), "\n",
    "values: ", values, "\n",
    "states: ", paste(states, collapse = " "), "\n",
    "actions: ", paste(actions, collapse = " "), "\n",
    "observations: ", paste(observations, collapse = " "), "\n"
  )
  
  
  ### starting beliefs
  if(!is.null(start)) { 
    ## if the starting beliefs are given by enumerating the probabilities for each state
    if (is.numeric(start)) {
      if (length(start) == length(states) && sum(start)==1) {
        code <- paste0(code,"start: ", format_fixed(start, digits), "\n")
      } else {
      ## this should be indices (pomdp_solve starts with 0)
      start_ids <- as.integer(abs(start)) - 1L
      if(all(start < 0)) 
        code <- paste0(code, "start include: ", paste(start_ids, collapse = " "), "\n")
      if(all(start > 0))
        code <- paste0(code, "start exclude: ", paste(start_ids, collapse = " "), "\n")
      else stop("Illegal specification of start. State ids start with 1 need to be all positive or negative (for exclusion).")
      }
    
    } else if(is.character(start))
      ## if the starting beliefs are given by a uniform distribution over all states
      if (length(start) == 1 && start[1] == "uniform") {
        code <- paste0(code,"start: ", paste(start, collapse = " "), "\n")
        
      } else if (start[1] != "-")  
        code <- paste0(code, "start include: ", paste(start, collapse = " "), "\n")
      else 
        code <- paste0(code, "start exclude: ", paste(start[-1], collapse = " "), "\n")
  } else stop("Illegal specification of start.")


  code <- paste0(code, "\n")
  
  ### Transition Probabilities
  
  ## if the transition probabilities are given in the general form
  if (is.data.frame(transition_prob)) {
    # checking if the number of the columns of the given data frame is 4
    if (ncol(transition_prob) != 4) {
      stop("the given data frame for the transition probabilities needs to have 4 columns including 'action', 'start-state','end-state','probability'")
    }
    
    # writing the transition probability lines
    for (i in 1:nrow(transition_prob)) {
      # fix indexing
      if(is.numeric(transition_prob[i,1])) transition_prob[i,1] <- transition_prob[i,1] -1    
      if(is.numeric(transition_prob[i,2])) transition_prob[i,1] <- transition_prob[i,1] -1    
      if(is.numeric(transition_prob[i,3])) transition_prob[i,1] <- transition_prob[i,1] -1    
      
      code <- paste0(code,"T: ", 
        transition_prob[i,1], " : ", 
        transition_prob[i,2], " : ", 
        transition_prob[i,3], " ",
        format_fixed(transition_prob[i,4], digits = digits),  
        "\n")
    }
  }else{
    ## if the transition probabilities are given in the form of action dependent matrices
    # checking if the number of the given transition probability matrices matches the number of actions
    if (length(transition_prob)!=number_of_actions) 
      stop("the number of given transition probability matrices does not match the number of actions")
    # writing the transition probability matrices
    for (a in actions) {
      code <- paste0(code, "T: ", a, "\n")
      
      if (is.character(transition_prob[[a]]) && length(transition_prob[[a]]) == 1)
        code <- paste0(code, transition_prob[[a]], "\n")
      else 
        code <- paste0(code, format_fixed(transition_prob[[a]], digits), "\n")
    }
  }
  code <- paste0(code, "\n")
  
  ### Observation Probabilities
  
  ## if the observation probabilities are given in the general form
  if (is.data.frame(observation_prob)) {
    # checking if the number of the columns of the given data frame is 4
    if (dim(observation_prob)[2] != 4) {
      stop("the given data frame for the observation probabilities needs to have 4 columns including 'action', 'end-state','observation','probability'")
    }
    
    # writing the transition probabilities lines
    for (i in 1:dim(observation_prob)[1]) {
      # fix indexing
      if(is.numeric(observation_prob[i,1])) observation_prob[i,1] <- observation_prob[i,1] -1    
      if(is.numeric(observation_prob[i,2])) observation_prob[i,1] <- observation_prob[i,1] -1    
      if(is.numeric(observation_prob[i,3])) observation_prob[i,1] <- observation_prob[i,1] -1    
      
      code <- paste0(code,"O: ", 
        observation_prob[i,1], " : ", 
        observation_prob[i,2], " : ", 
        observation_prob[i,3], " ", 
        format_fixed(observation_prob[i,4], digits = digits), "\n")
    }
  }else{
    ## if the observation probabilities are given in the form of action dependent matrices
    # checking if the number of the given observation probability matrices matches the number of actions
    if (length(observation_prob)!=number_of_actions) {
      stop("the number of given observation probability matrices does not match the number of actions")
    }
    # writing the observation probability matrices
    for (a in actions) {
      code <- paste0(code,"O: ", a, "\n")
      
      if (is.character(observation_prob[[a]]) && length(observation_prob[[a]]) == 1) {
        code <- paste0(code, observation_prob[[a]], "\n")
      } else {
        if(any(dim(observation_prob[[a]]) != c(number_of_states, number_of_observations)))
          stop("Observation matrix for action '", actions[i], "' is not of size # of states times # of observations!")
        code <- paste0(code, format_fixed(observation_prob[[a]], digits), "\n")
      }
    }
  }
  
  code <- paste0(code, "\n")
  
  ### Rewards/Costs
  
  ## if the rewards are given in the general form
  if (is.data.frame(reward)) {
    # checking if the number of the columns of the given data frame is 5
    if (dim(reward)[2] != 5) {
      stop("the given data frame for the Rewards needs to have 5 columns including 'action', 'start-state','end-state','observation', 'values'")
    }
    
    # writing the reward lines
    for (i in 1:nrow(reward)) {
      code <- paste0(code,"R: ", 
        reward[i,1], " : ", 
        reward[i,2], " : ", 
        reward[i,3], " : ", 
        reward[i,4], " ",
        format_fixed(reward[i,5], digits),  "\n")
    }
  }else{
    
    ## if the rewards are given in the form of action and start-state dependent matrices
    # checking if the number of the given reward matrices matches the number of actions and states
    if (length(reward)!= (number_of_actions)) {
      stop("the number of given list matrices does not match the number of actions")
    }
    for (i in 1:number_of_actions) {
      if (length(reward[[actions[i]]])!= (number_of_states)) {
        stop("the number of given reward matrices for action ", i ," does not match the number of states")
      }
    }
    # writing the reward matrices
    for (i in 1:number_of_actions) {
      for (j in 1:number_of_states) {
        code <- paste0(code,"R: ", actions[i], ":" , states[j], "\n")
        
        if (is.character(reward[[actions[i]]][[states[j]]]) && length(reward[[actions[i]]][[states[j]]]) == 1){
          code <- paste0(code, reward[[actions[i]]][[states[j]]] , "\n")
        } 
        else {
          code <- paste0(code, format_fixed(reward[[actions[i]]][[states[j]]], digits), "\n")
        }
      }
    }
    
    code <- paste0(code, "\n")
    
  }
  
  ### saving the POMDP file
  cat(code, file = file)
}

#' @rdname write_POMDP
#' @export
read_POMDP <- function(file) {
    problem <- readLines(file)  
    
    get_vals <- function(var, number = FALSE) {
      ind <- grep(paste0(var,":"), problem)
      if(length(ind) == 0) return(NULL)
      
      vals <- strsplit(trimws(problem[[ind]]), "\\s+")[[1]][-1]
      
      # the data may be in the next line
      if(length(vals) == 0) vals <- strsplit(problem[[ind+1]], "\\s+")[[1]]
      
      # numbers?
      vals <- type.convert(vals, as.is = TRUE)
      
      # create labels if just the number is mentioned
      if(number && length(vals) == 1 && is.numeric(vals)) 
        vals <- paste0(substr(var, 1, 1), seq(vals)) 
    vals
    }
    
    model <- structure(list(
      name = file,
      states = get_vals("states", number = TRUE),
      observations = get_vals("observations", number = TRUE),
      actions = get_vals("actions", number = TRUE),
      start = get_vals("start"),
      discount = get_vals("discount"),
      problem = structure(problem, class = "text")
    ), class = "POMDP_model")
    
    structure(list(model = model), class = "POMDP")
}    
