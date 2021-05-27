# color helper

.get_colors_descrete <- function(n, col = NULL) {
  if (is.null(col)) {
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
    if (n <= 9)
      col <- col[1:n]
    else
      col <- grDevices::rainbow(n)
  }
  
  if (length(col) != n)
    stop("Number of colors does not match.")
  
  col
}

.get_colors_cont <- function(val, col = NULL) {
  if (is.null(col))
    col <- c("#377EB8", "#E41A1C") # blue -> red
  grDevices::rgb(grDevices::colorRamp(col, space = "Lab")((val - min(val, na.rm = TRUE)) / (
    max(val, na.rm = TRUE) - min(val, na.rm = TRUE)
  )) / 255)
}
