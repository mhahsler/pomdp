#' @keywords internal 
#'
#' @section Key functions:
#' - Problem specification: [POMDP], [MDP]
#' - Solvers: [solve_POMDP()], [solve_MDP()], [solve_SARSOP()]
#'
#' @import Rcpp
#' @importFrom Matrix spMatrix crossprod coerce Math Math2 cBind rBind nnzero
#' @importFrom methods as
#' @importFrom utils head tail read.table type.convert
#' @importFrom foreach foreach times %dopar% getDoParWorkers
#' @useDynLib pomdp, .registration=TRUE
"_PACKAGE"

.onAttach <-function(libname, pkgname) {
  ### silence warning for no backend
  if (!foreach::getDoParRegistered())
    foreach::registerDoSEQ()
}
