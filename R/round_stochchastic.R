#' Round a stochastic vector or a row-stochastic matrix
#'
#' Rounds a vector such that the sum of 1 is preserved. Rounds a matrix such
#' that the rows still sum up to 1.
#'
#' Rounds and adjusts one entry such that the rounding error is the smallest.
#'
#' @param x a stochastic vector or a row-stochastic matrix.
#' @param digits number of digits for rounding.
#' @return The rounded vector or matrix.
#' @seealso \link{round}
#' @examples
#' # a vector that is off by 1e-8
#' x <- c(0.25 + 1e-8, 0.25, 0.5)
#' 
#' round_stochastic(x)
#' round_stochastic(x, digits = 2)
#' round_stochastic(x, digits = 1)
#' round_stochastic(x, digits = 0)
#' @export
round_stochastic <- function(x, digits = getOption("digits")) {
  if (is.matrix(x))
    t(apply(
      x,
      MARGIN = 1,
      .round_preserve_sum,
      digits = digits
    ))
  else
    .round_preserve_sum(x, digits = digits)
}

# Round a stochastic vector
# from https://github.com/larmarange/JLutils/blob/master/R/round_preserve_sum.R
.round_preserve_sum <- function(x, digits = getOption("digits")) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  r <- y / up
  if (!sum1(r, digits))
    warning("The rounded vector is not stochastic.")
  r
}

sum1 <- function(x, digits = getOption("digits")) {
  if(is.matrix(x))
    all(apply(x, MARGIN = 1, sum1))
  else
    zapsmall(sum(x), digits) == 1
}
