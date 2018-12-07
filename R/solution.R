solution <- function(x) {
  if(!is(x, "POMDP")) stop("x needs to be a POMDP object!")
  x$solution
}