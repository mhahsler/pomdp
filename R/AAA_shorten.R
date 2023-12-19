
# shorten a string so it fits in a line.
shorten <- function(x, n, ellipsis = "...") {
  if (n < 0L)
    n <- getOption("width") + n - nchar(ellipsis)
  
  if (nchar(x) > n)
    x <- paste0(substr(x, 1, n), ellipsis)
  
  x
}