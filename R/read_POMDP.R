## read a POMDP model description as a .POMDP file.
.parse_POMDP_model_file <- function(file) {
    problem <- readLines(file)  
    
    get_vals <- function(var, number = FALSE) {
      ind <- grep(paste0(var,":"), problem)
      if(length(ind) == 0) return(NULL)
      
      vals <- strsplit(problem[[ind]], "\\s+")[[1]][-1]
      
      # the data may be in the next line
      if(length(vals) == 0) vals <- strsplit(problem[[ind+1]], "\\s+")[[1]]
      
      # numbers?
      vals <- type.convert(vals, as.is = TRUE)
      
      # create labels if just the number is mentioned
      if(number && length(vals) == 1 && is.numeric(vals)) 
        vals <- paste0(substr(var, 1, 1), seq(vals)) 
    vals
    }
    
    structure(list(
      name = file,
      states = get_vals("states", number = TRUE),
      observations = get_vals("observations", number = TRUE),
      actions = get_vals("actions", number = TRUE),
      start = get_vals("start"),
      discount = get_vals("discount"),
      problem = structure(problem, class = "text")),
      class = "POMDP_model"
    )
}    

  
## helpers to read pomdp-solve files
.get_alpha_file <- function(file_prefix, model, number = "") {  
  filename <- paste0(file_prefix, '-0.alpha',number)
  ## importing alpha file
  alpha <- readLines(filename)
  alpha <- alpha[seq(2, length(alpha), 3)]
  alpha <- do.call(rbind, lapply(alpha, function(a) as.numeric(strsplit(a, " ")[[1]])))
  colnames(alpha) <- model$model$states
  alpha
}

