# plot a projection of the value function

### FIXME: Add ternary plots for 3 states

plot_value_function <- function(model, projection = 1:2, epoch = 1, ylim = NULL, legend = TRUE) {

  .solved_POMDP(model)

  if(is.character(projection)) projection <- pmatch(projection, model$model$states)
  if(length(projection) != 2) stop("Value function needs to be projected onto two states for plotting.")
  
  alpha <- model$solution$alpha
  pg <- model$solution$pg
   
  # finite-horizon
  if(is.list(alpha)) {
    if(epoch > length(alpha)) stop("The solution does not contain that many epochs. Either the horizon was set to less epochs or the solution converged earlier.")
    alpha <- alpha[[epoch]]
    pg <- pg[[epoch]]
  }
  
  alpha <- alpha[,projection, drop = FALSE]
  if(is.null(ylim)) ylim <- c(min(alpha), max(alpha))
  
  plot(NA, xlim = c(0, 1), ylim = ylim, 
    xlab = paste0("Belief space", 
      ifelse(length(projection) < length(model$model$states), " (projected)", "")), 
    ylab = "Value function", axes = FALSE)
  axis(2)
  axis(1, at = c(0,1), labels = model$model$states[projection])
  axis(1, at = .5, .5)
  box() 
  
  for(i in 1:nrow(alpha)) lines(x = c(0, 1), y = c(alpha[i,1], alpha[i,2]), col = i, xpd = FALSE)
  
  if(legend) legend("topright", legend = 
      paste0(1:nrow(alpha),": ", pg[,"action"]), col = 1:nrow(alpha), lwd=1, bty = "n",
    title = "Action")
  
  ### use ggplot instead
  #alpha <- cbind(as.data.frame(alpha), Action = factor(paste0(1:nrow(alpha), ": ", pg[,"action"])))
  #ggplot() + geom_segment(data = alpha, mapping = 
  #    aes_(x=0, y=as.name(colnames(alpha)[1]), xend=1, yend=as.name(colnames(alpha)[2]), color = quote(Action))
  #  ) + coord_cartesian(ylim = c(0, 15)) +
  #   ylab("Reward") + xlab("Belief")
  
}

