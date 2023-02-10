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
#' # regular rounding would not sum up to 1 
#' x <- c(0.333, 0.334, 0.333)
#' 
#' round_stochastic(x)
#' round_stochastic(x, digits = 2)
#' round_stochastic(x, digits = 1)
#' round_stochastic(x, digits = 0)
#' 
#' 
#' # round a stochastic matrix
#' m <- matrix(runif(15), ncol = 3)
#' m <- sweep(m, 1, rowSums(m), "/")
#' 
#' m
#' round_stochastic(m, digits = 2)
#' round_stochastic(m, digits = 1)
#' round_stochastic(m, digits = 0)
#' @export
round_stochastic <- function(x, digits = 7) {
  if (is.matrix(x))
    t(apply(
      x,
      MARGIN = 1,
      round_stochastic_int,
      digits = digits
    ))
  else
    round_stochastic_int(x, digits = digits)
}

# Round a stochastic vector
round_stochastic_int <- function(x, digits = 7) {
  # handle impossible beliefs
  if (any(is.na(x)))
    return (x)
  
  x[x<0] <- 0
  
  #x / sum(x)
  xr <- round(x, digits = digits)
  
  # the sum can now be less
  s <- sum(xr)
  if (s != 1) {
    #inc_id <- which.max(x - xr)
    ### take it from the largest
    inc_id <- which.max(xr)
    xr[inc_id] <- xr[inc_id] + 1 - s
  }
  
  if (any(xr < 0) || !sum1(xr))
    stop("Stochastic rounding failed!")
  
  xr
}


# check if vector sums to 1
sum1 <- function(x, digits = getOption("digits")) {
  if(is.matrix(x))
    all(apply(x, MARGIN = 1, sum1))
  else
    zapsmall(sum(x), digits) == 1
}

