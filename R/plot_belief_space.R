
### FIXME: Add belief points
plot_belief_space <- function(model, projection = c(1,2), n = 100, 
  what = c("action", "pg_node", "reward"), legend = TRUE) {
  p <- sample_belief_space(model, n = n, projection = projection)
  alpha <- p$belief[,projection]
  
  what <- match.arg(what)
  val <- p$optimal[[what]]
  if(is.factor(val)) col <- as.integer(val) + 1L
  else col <- rgb(colorRamp(c("blue", "red"), space = "Lab")((val-min(val))/(max(val)-min(val)))/255) 
  
  
  if(length(projection) == 3) {
    TernaryPlot(
      alab=paste(colnames(alpha)[1], "\u2192"), 
      blab=paste(colnames(alpha)[2], "\u2192"), 
      clab=paste("\u2190", colnames(alpha)[3]), 
      grid.lines=10, grid.lty='dotted',
      grid.minor.lines=1, grid.minor.lty='dotted')  
    TernaryPoints(alpha, pch = 20, col = col)
    
  } else if(length(projection) == 2) {
    plot(NA, 
      xlim = c(0,1), ylim = c(0,2),
      axes = FALSE, xlab = NA, ylab = NA)
    
    points(alpha[,1], rep(0, times = nrow(alpha)), col = col, pch = 20) 
    
    axis(1, at = c(0,1), labels = colnames(alpha), xaxs = "i")
    axis(1, at = .5, .5) 
    
  } else stop("projection needs to be 2d or 3d.")
  
  if(legend) 
    if(is.integer(col)) 
      legend("topright", legend = levels(p$optimal[[what]]), 
        pch = 20, 
        col = seq(length(levels(p$optimal[[what]])))+1, 
        bty = "n", 
        title = what)     
  else{
    legend("topright",
      legend = round(c(max(val), rep(NA, 9), min(val)), 2),
      fill = rev(colorRampPalette(c("blue", "red"))(11)),
      border = NA,
      y.intersp = .5,
      bty = "n", 
      title = what) 
  }
  
}

