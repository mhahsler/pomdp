#' Round a stochastic vector 
#' from https://github.com/larmarange/JLutils/blob/master/R/round_preserve_sum.R
.round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  r <- y / up
  if(zapsmall(sum(r)) != 1) warning("The rounded vector is not stochastic.")
  r
}

round_stochastic <- function(x, digits = 3) {
  if(is.matrix(x)) t(apply(x, MARGIN = 1, .round_preserve_sum, digits = digits))
  else .round_preserve_sum(x, digits = digits)
}
