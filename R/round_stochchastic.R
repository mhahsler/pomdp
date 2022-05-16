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
#'
#' x <- c(0.25, 0.25, 0.5)
#' round_stochastic(x, 2)
#' round_stochastic(x, 1)
#' round_stochastic(x, 0)
#'
#' @export
round_stochastic <- function(x, digits = 3) {
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
.round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  r <- y / up
  if (zapsmall(sum(r)) != 1)
    warning("The rounded vector is not stochastic.")
  r
}

sum1 <- function(x) {
  if(is.matrix(x))
    all(apply(x, MARGIN = 1, sum1))
  else
    zapsmall(sum(x) - 1) == 0
}
