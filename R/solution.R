solution <- function(x) {
  if(!inherits(x, "POMDP")) stop("x needs to be a POMDP object!")
  x$solution
}
