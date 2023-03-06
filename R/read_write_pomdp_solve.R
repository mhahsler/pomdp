# helpers to read/write pomdp-solve files

# alpha file is a matrix with # of states columns
.write_alpha_file <- function(file_prefix, alpha, digits = 7) {
  filename <- paste0(file_prefix, '_terminal_values.alpha') 
  pomdpSolve::write_terminal_values(filename, alpha = alpha, digits = digits)
  
  filename
}

.get_alpha_file <- function(file_prefix, model, number = "") {
  alpha <- pomdpSolve::read_alpha_file(paste0(file_prefix, '-0.alpha', number))
  colnames(alpha) <- model$states
  alpha
}

## importing pg file
.get_pg_file <- function(file_prefix, model, number = "") {
  filename <- paste0(file_prefix, '-0.pg', number)
  pg <- pomdpSolve::read_pg_file(filename)
  
  # renaming the columns and actions
  colnames(pg) <-
    c("node", "action", as.character(model$observations))
  pg[, 2] <- factor(pg[, 2], levels = seq(length(model$actions)), labels = model$actions)
  pg
}

# importing belief file (used belief points) if it exists (only grid method)
.get_belief_file <- function(file_prefix, model) {
  belief <- pomdpSolve::read_belief_file(paste0(file_prefix, '-0.belief'))
  if (!is.null(belief)) 
    colnames(belief) <- as.character(model$states)
  belief
}

# write belief file to specify which belief points the grid method should use.
.write_grid_file <- function(file_prefix, belief, digits = 7) {
  filename <- paste0(file_prefix, '.grid')
  pomdpSolve::write_grid_file(filename, belief, digits = digits)
  filename
}
