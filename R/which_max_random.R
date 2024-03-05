# this is borrowed from nnet::which.is.max
which.max.random <- function (x)
{
  mx <- x == max(x, na.rm = TRUE)
  mx[is.na(mx)] <- FALSE
  y <- seq_along(x)[mx]
  if (length(y) > 1L)
    sample(y, 1L)
  else
    y
}