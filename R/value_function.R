#' Value Function
#'
#' Extracts the value function from a solved model.

#' Extracts the alpha vectors describing the value function. This is similar to [policy()] which in addition returns the
#' action prescribed by the solution.
#'
#' Plots the value function of a POMDP solution as a line plot. The solution is
#' projected on two states (i.e., the belief for the other states is held
#' constant at zero). The value function can also be visualized using [plot_belief_space()].
#'
#' @family policy
#' @family POMDP
#' @family MDP
#'
#' @param model a solved [POMDP] or [MDP].
#' @param projection Sample in a projected belief space. See [projection()] for details.
#' @param epoch the value function of what epoch should be plotted? Use 1 for
#'   converged policies.
#' @param ylim the y limits of the plot.
#' @param legend logical; add a legend to show the actions?
#' @param col potting colors.
#' @param lwd line width.
#' @param lty line type.
#' @param ylab label for the y-axis.
#' @param ... additional arguments are passed on to [stats::line()]`.
#'
#' @returns the function as a matrix with alpha vectors as rows.
#'
#' @author Michael Hahsler
#' @keywords hplot
#' @examples
#' data("Tiger")
#' sol <- solve_POMDP(model = Tiger)
#' sol
#'
#' # value function for the converged solution
#' value_function(sol)
#'
#' plot_value_function(sol, ylim = c(0,20))
#'
#' ## finite-horizon problem
#' sol <- solve_POMDP(model = Tiger, horizon = 3, discount = 1,
#'   method = "enum")
#' sol
#'
#' # inspect the value function for all epochs
#' value_function(sol)
#'
#' plot_value_function(sol, epoch = 1, ylim = c(-5, 25))
#' plot_value_function(sol, epoch = 2, ylim = c(-5, 25))
#' plot_value_function(sol, epoch = 3, ylim = c(-5, 25))
#'
#' \dontrun{
#' # using ggplot2 to plot the value function for epoch 3
#' library(ggplot2)
#' pol <- policy(sol)[[3]]
#' ggplot(pol) +
#'  geom_segment(aes(x = 0, y = `tiger-left`, xend = 1, yend = `tiger-right`, color = action)) +
#'  coord_cartesian(ylim = c(-5, 15)) + ylab("Reward") + xlab("Belief")
#' }
#' @importFrom graphics plot barplot box lines text
#' @export
value_function <- function(model) {
  UseMethod("value_function")
}

#' @export
value_function.MDP <- function(model) {
  is_solved_MDP(model, stop = TRUE)
  
  return(lapply(policy(model), "[[", "U"))
}

#' @export
value_function.POMDP <- function(model) {
  is_solved_POMDP(model, stop = TRUE)
  .check_valid_value_function(model)
  
  return(model$solution$alpha)
}


.check_valid_value_function <-
  function(x, warning = TRUE, stop = FALSE) {
    is_solved_POMDP(x, stop = TRUE)
    
    msg_f <- function(...) {
      
    }
    if (warning)
      msg_f <- base::warning
    if (stop)
      msg_f <- base::stop
    
    if (x$solution$method == "manual" && !x$solution$converged) {
      msg_f("Value function (alpha vectors) may not be valid for a manual policy!")
      
      return(FALSE)
    }
    
    if (x$solution$method == "grid" &&
        is.finite(x$horizon) &&
        !x$solution$converged &&
        any(reward_matrix(x, sparse = TRUE)$value < 0)) {
      msg_f(
        "Value function (alpha vectors) may not be valid for a finite horizon unconverged solution of method 'grid' with neg. rewards!"
      )
      
      return(FALSE)
    }
    
    return(TRUE)
  }


#' @rdname value_function
#' @export
plot_value_function <- function(model,
                                projection = NULL,
                                epoch = 1,
                                ylim = NULL,
                                legend = TRUE,
                                col = NULL,
                                lwd = 1,
                                lty = 1,
                                ylab = "Reward",
                                ...) {
  UseMethod("plot_value_function")
}

#' @export
plot_value_function.MDP <-
  function(model,
           projection = NULL,
           epoch = 1,
           ylim = NULL,
           legend = TRUE,
           col = NULL,
           lwd = 1,
           lty = 1,
           ylab = "Reward",
           ...) {
    is_solved_MDP(model, stop = TRUE)
    
    policy <- policy(model)[[epoch]]
    
    mid <- barplot(
      policy$U,
      col = col,
      ylab = ylab,
      xlab = "State",
      names.arg = paste(model$states),
      ...
    )
    
    if (legend)
      text(
        x = mid,
        y = 0,
        labels = policy$action,
        srt = 90,
        adj = c(-.1, .5)
      )
    invisible(NULL)
  }

#' @export
plot_value_function.POMDP <-
  function(model,
           projection = NULL,
           epoch = 1,
           ylim = NULL,
           legend = TRUE,
           col = NULL,
           lwd = 1,
           lty = 1,
           ylab = "Reward",
           ...) {
    is_solved_POMDP(model, stop = TRUE)
    .check_valid_value_function(model)
    
    projection <- projection(projection, model)
    
    if (sum(is.na(projection)) != 2L)
      stop("Value function needs to be projected onto two states for plotting.")
    
    alpha <- model$solution$alpha
    pg <- model$solution$pg
    
    if (epoch > length(alpha))
      stop(
        "The solution does not contain that many epochs. ",
        "Either the horizon was set to less epochs or the solution converged earlier."
      )
    alpha <- alpha[[epoch]]
    pg <- pg[[epoch]]
    
    col <- colors_discrete(nrow(alpha), col)
    lwd <- rep(lwd, length.out = nrow(alpha))
    lty <- rep(lty, length.out = nrow(alpha))
    
    remainder_proj <- 1 - sum(projection, na.rm = TRUE)
    plot_dim_ids <- is.na(projection)
    
    bel_left <- projection
    bel_left[plot_dim_ids] <- c(remainder_proj, 0)
    val_left <- alpha %*% bel_left
    bel_right <- projection
    bel_right[plot_dim_ids] <- c(0, remainder_proj)
    val_right <- alpha %*% bel_right
    
    if (is.null(ylim)) {
      ylim <- range(c(val_left, val_right))
      if (ylim[1] > 0)
        ylim[1] <- 0
      ylim <- ylim + c(0, diff(ylim) * .2)
    }
    
    plot(
      NA,
      xlim = c(0, 1),
      ylim = ylim,
      xlab = paste0("Belief space",
                    ifelse(
                      length(projection) < length(model$states),
                      " (projected)",
                      ""
                    )),
      ylab = ylab,
      axes = FALSE
    )
    axis(1,
         at = c(0, 1, .5),
         labels = c(model$states[plot_dim_ids], NA))
    axis(2)
    box()
    
    for (i in 1:nrow(alpha))
      lines(
        ### fix with proj sum...
        x = (c(-remainder_proj, remainder_proj) + 1) / 2,
        y = c(val_left[i, 1], val_right[i, 1]),
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
    
    invisible(NULL)
  }
