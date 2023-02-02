#' Default Colors for Visualization in Package pomdp
#'
#' Default discrete and continuous colors used in pomdp for states (nodes), beliefs and values.
#'
#' @name colors
#'
#' @param n number of states.
#' @param col custom color palette. `colors_discrete()` uses the first n colors.  
#'  `colors_continuous()` uses these colors to calculate a palette (see [grDevices::colorRamp()])
#'
#' @returns `colors_discrete()` returns a color palette and 
#'  `colors_continuous()` returns the colors associated with the supplied values.
#' @examples
#' colors_discrete(5)
#'
#' colors_continuous(runif(10))
#' @export
colors_discrete <- function(n, col = NULL) {
  if (is.null(col))
    # colorbrewer Set 1
    col <-
      c(
        "#E41A1C",
        "#377EB8",
        "#4DAF4A",
        "#984EA3",
        "#FF7F00",
        "#FFFF33",
        "#A65628",
        "#F781BF",
        "#999999"
      )
  
  if (n <= length(col))
    col <- col[1:n]
  else
    col <- grDevices::rainbow(n)
  
  col
}

#' @rdname colors
#' @param val a vector with values to be translated to colors.
#' @export
colors_continuous <- function(val, col = NULL) {
  if (is.null(col))
    col <- c("#377EB8", "#E41A1C") # blue -> red
  grDevices::rgb(grDevices::colorRamp(col, space = "Lab")((val - min(val, na.rm = TRUE)) / (
    max(val, na.rm = TRUE) - min(val, na.rm = TRUE)
  )) / 255)
}
