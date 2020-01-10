
### FIXME: Add belief points
plot_belief_space <- function(model, projection = NULL, epoch = 1, n = 100, random = FALSE, 
  what = c("action", "pg_node", "reward"), legend = TRUE, pch = 20, col = NULL, ...) {
  
  what <- match.arg(what)
  if(is.null(projection)) projection <- 1:min(length(model$model$states), 3)
 
  p <- sample_belief_space(model, projection = projection, epoch = epoch, n = n, random = random)
  belief <- p$belief[,projection]
  val <- p$optimal[[what]]
  
  # col ... palette used for legend
  # cols ... colors for all points
  if(is.factor(val)) {
    col <- .get_colors_descrete(length(levels(val)), col)
    cols <- col[as.integer(val)]
  } else {
    col <- .get_colors_cont(seq(0,1, length.out = 11), col)
    cols <- .get_colors_cont(val, col)
  }
    
  if(length(projection) == 3) {
    Ternary::TernaryPlot(
      alab=paste(colnames(belief)[1], "\u2192"), 
      blab=paste(colnames(belief)[2], "\u2192"), 
      clab=paste("\u2190", colnames(belief)[3]), 
      grid.lines=10, grid.lty='dotted',
      grid.minor.lines=1, grid.minor.lty='dotted')  
    
    Ternary::TernaryPoints(belief, pch = pch, col = cols, ...)
    
   # if(random) Ternary::TernaryPoints(belief, pch = pch, col = cols, ...)
   # else {
   #   values <- rbind(
   #     x = attr(p$belief, "TernaryTriangleCenters")["x",], 
   #     y = attr(p$belief, "TernaryTriangleCenters")["y",], 
   #     z = val, 
   #     down = attr(p$belief, "TernaryTriangleCenters")["triDown",]) 
   #   Ternary::ColourTernary(values, spectrum = col)
   # }
    
  } else if(length(projection) == 2) {
    plot(NA, 
      xlim = c(0,1), ylim = c(0,2),
      axes = FALSE, xlab = NA, ylab = NA)
    
    points(belief[,1], rep(0, times = nrow(belief)), col = cols, pch = pch, ...) 
    
    axis(1, at = c(0,1), labels = colnames(belief), xaxs = "i")
    axis(1, at = .5, .5) 
    
  } else stop("projection needs to be 2d or 3d.")
  
  if(legend) 
    if(is.factor(val)) 
      legend("topright", legend = levels(p$optimal[[what]]), 
        pch = pch, 
        col = col, 
        bty = "n", 
        title = what)     
  else{
    legend("topright",
      legend = round(c(max(val), rep(NA, 9), min(val)), 2),
      fill = rev(col),
      border = NA,
      y.intersp = .5,
      bty = "n", 
      title = what) 
  }
 
  invisible(p) 
}

