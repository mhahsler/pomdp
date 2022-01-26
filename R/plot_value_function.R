#' Plot the Value Function of a POMDP Solution
#'
#' Plots the value function of a POMDP solution as a line plot. The solution is
#' projected on two states (i.e., the belief for the other states is held
#' constant at zero).
#'
#' @param model a solved [POMDP].
#' @param projection index or name of two states for the projection.
#' @param epoch the value function of what epoch should be plotted? Ignored
#' for infinite-horizon solutions.
#' @param ylim the y limits of the plot.
#' @param legend logical; add a legend?
#' @param col potting colors.
#' @param lwd line width.
#' @param lty line type.
#' @param ... additional arguments are passed on to [stats::line()]`.
#' @author Michael Hahsler
#' @keywords hplot
#' @examples
#' data("Tiger")
#' sol <- solve_POMDP(model = Tiger)
#' sol
#'
#' plot_value_function(sol, ylim = c(0,20))
#'
#' ## finite-horizon
#' sol <- solve_POMDP(model = Tiger, horizon = 3, discount = 1,
#'   method = "enum")
#' sol
#'
#' plot_value_function(sol, epoch =1, ylim = c(-5, 25))
#' plot_value_function(sol, epoch =2, ylim = c(-5, 25))
#' plot_value_function(sol, epoch =3, ylim = c(-5, 25))
#'
#' @export
plot_value_function <-
  function(model,
    projection = 1:2,
    epoch = 1,
    ylim = NULL,
    legend = TRUE,
    col = NULL,
    lwd = 1,
    lty = 1,
    ...) {
    if (inherits(model, "MDP")) {
      .solved_MDP(model)
      
      policy <- policy(model)[[epoch]]
      
      barplot(
        policy$U,
        ylab = "Reward",
        names.arg = paste(model$model$states, "\n", policy$action),
        ...
      )
      mtext(
        "State",
        side = 1,
        line = 0,
        adj = 1,
        at = 0
      )
      mtext(
        "Action",
        side = 1,
        line = 1,
        adj = 1,
        at = 0
      )
    } else {
      .solved_POMDP(model)
      
      if (is.character(projection))
        projection <- pmatch(projection, model$model$states)
      if (length(projection) != 2)
        stop("Value function needs to be projected onto two states for plotting.")
      
      alpha <- model$solution$alpha
      pg <- model$solution$pg
      
      if (epoch > length(alpha))
        stop(
          "The solution does not contain that many epochs. Either the horizon was set to less epochs or the solution converged earlier."
        )
      alpha <- alpha[[epoch]]
      pg <- pg[[epoch]]
      
      alpha <- alpha[, projection, drop = FALSE]
      if (is.null(ylim))
        ylim <- c(min(alpha), max(alpha))
      col <- .get_colors_descrete(nrow(alpha), col)
      lwd <- rep(lwd, length.out = nrow(alpha))
      lty <- rep(lty, length.out = nrow(alpha))
      
      plot(
        NA,
        xlim = c(0, 1),
        ylim = ylim,
        xlab = paste0("Belief space",
          ifelse(
            length(projection) < length(model$model$states),
            " (projected)",
            ""
          )),
        ylab = "Reward",
        axes = FALSE
      )
      axis(2)
      axis(1, at = c(0, 1), labels = model$model$states[projection])
      axis(1, at = .5, .5)
      box()
      
      for (i in 1:nrow(alpha))
        lines(
          x = c(0, 1),
          y = c(alpha[i, 1], alpha[i, 2]),
          col = col[i],
          lwd = lwd[i],
          lty = lty[i],
          xpd = FALSE,
          ...
        )
      
      if (legend)
        legend(
          "topright",
          legend =
            paste0(1:nrow(alpha), ": ", pg[, "action"]),
          col = col,
          bty = "n",
          lwd = lwd,
          lty = lty,
          title = "Action",
          ...
        )
      
      ### use ggplot instead
      #alpha <- cbind(as.data.frame(alpha), Action = factor(paste0(1:nrow(alpha), ": ", pg[,"action"])))
      #ggplot() + geom_segment(data = alpha, mapping =
      #    aes_(x=0, y=as.name(colnames(alpha)[1]), xend=1, yend=as.name(colnames(alpha)[2]), color = quote(Action))
      #  ) + coord_cartesian(ylim = c(0, 15)) +
      #   ylab("Reward") + xlab("Belief")
    }
  }
