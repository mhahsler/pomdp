#' Plot a 2D or 3D Projection of the Belief Space
#'
#' Plots the optimal action, the node in the policy graph or the reward for a
#' given set of belief points on a line (2D) or on a ternary plot (3D). If no
#' points are given, points are sampled using a regular arrangement or randomly
#' from the (projected) belief space.
#'
#' @family policy
#' @family POMDP
#'
#' @param model a solved [POMDP].
#' @param projection Sample in a projected belief space. See [projection()] for details.
#' @param epoch display this epoch.
#' @param sample a matrix with belief points as rows or a character string
#' specifying the `method` used for [sample_belief_space()].
#' @param n number of points sampled.
#' @param what what to plot.
#' @param legend logical; add a legend? If the legend is covered by the plot then you
#' need to increase the plotting region of the plotting device.
#' @param pch plotting symbols.
#' @param col plotting colors.
#' @param jitter jitter amount for 2D belief spaces (good values are between 0 and 1, while using `ylim = c(0,1)`).
#' @param oneD plot projections on two states in one dimension.
#' @param ...  additional arguments are passed on to `plot` for 2D or
#' `TerneryPlot` for 3D.
#' @return Returns invisibly the sampled points.
#' @author Michael Hahsler
#' @keywords hplot
#' @examples
#' # two-state POMDP
#' data("Tiger")
#' sol <- solve_POMDP(Tiger)
#'
#' plot_belief_space(sol)
#' plot_belief_space(sol, oneD = FALSE)
#' plot_belief_space(sol, n = 10)
#' plot_belief_space(sol, n = 100, sample = "random")
#'
#' # plot the belief points used by the grid-based solver
#' plot_belief_space(sol, sample = sol $solution$belief_points_solver)
#'
#' # plot different measures
#' plot_belief_space(sol, what = "pg_node")
#' plot_belief_space(sol, what = "reward")
#'
#' # three-state POMDP
#' # Note: If the plotting region is too small then the legend might run into the plot
#' data("Three_doors")
#' sol <- solve_POMDP(Three_doors)
#' sol
#'
#' # plotting needs the suggested package Ternary
#' if ("Ternary" %in% installed.packages()) {
#' plot_belief_space(sol)
#' plot_belief_space(sol, n = 10000)
#' plot_belief_space(sol, what = "reward", sample = "random", n = 1000)
#' plot_belief_space(sol, what = "pg_node", n = 10000)
#' 
#' # holding tiger-left constant at .5 follows this line in the ternary plot 
#' Ternary::TernaryLines(list(c(.5, 0, .5), c(.5, .5, 0)), col = "black", lty = 2)
#' # we can plot the projection for this line 
#' plot_belief_space(sol, what = "pg_node", n = 1000, projection = c("tiger-left" = .5))
#'
#' # plot the belief points used by the grid-based solver
#' plot_belief_space(sol, sample = sol$solution$belief_points_solver, what = "pg_node")
#'
#' # plot the belief points obtained using simulated trajectories with an epsilon-greedy policy.
#' # Note that we only use n = 50 to save time.
#' plot_belief_space(sol, 
#'   sample = simulate_POMDP(sol, n = 50, horizon = 100,
#'     epsilon = 0.1, return_beliefs = TRUE)$belief_states)
#' }
#'
#' # plot a 3-state belief space using ggtern (ggplot2)
#' \dontrun{
#' library(ggtern)
#' samp <- sample_belief_space(sol, n = 1000)
#' df <- cbind(as.data.frame(samp), reward_node_action(sol, belief = samp))
#' df$pg_node <- factor(df$pg_node)
#' 
#' ggtern(df, aes(x = `tiger-left`, y = `tiger-center`, z = `tiger-right`)) +
#'   geom_point(aes(color = pg_node), size = 2)
#'
#' ggtern(df, aes(x = `tiger-left`, y = `tiger-center`, z = `tiger-right`)) +
#'   geom_point(aes(color = action), size = 2)
#'
#' ggtern(df, aes(x = `tiger-left`, y = `tiger-center`, z = `tiger-right`)) +
#'   geom_point(aes(color = reward), size = 2)
#' }
#' @importFrom graphics plot axis legend
#' @export
plot_belief_space <-
  function(model,
    projection = NULL,
    epoch = 1,
    sample = "regular",
    n = 100,
    what = c("action", "pg_node", "reward"),
    legend = TRUE,
    pch = 20,
    col = NULL,
    jitter = 0,
    oneD = TRUE,
    ...) {
    # sample: a matrix with belief points or a character string passed on to sample_belief_space as method.
    # E.g., "regular", "random", ...
    
    what <- match.arg(what)
    projection <- projection(projection, model)
    
    if (is.character(sample))
      sample <-  sample_belief_space(model,
        projection = projection,
        n = n,
        method = sample)
    else
      if (any(!is.na(projection)))
        stop("A given sample cannot be projected!")
   
    # only warn for reward
    if (what == "reward") 
      .check_valid_value_function(model)
    
    val <-
      suppressWarnings(reward_node_action(model, belief = sample, epoch = epoch))[[what]]
    if (what == "pg_node")
      val <-
      factor(val, levels = seq_len(nrow(model$solution$pg[[epoch]])))
    
    # col ... palette used for legend
    # cols ... colors for all points
    if (is.factor(val)) {
      # actions and pg_node are factor
      col <- colors_discrete(length(levels(val)), col)
      cols <- col[as.integer(val)]
    } else {
      col <- colors_continuous(seq(0, 1, length.out = 11), col)
      cols <- colors_continuous(val, col)
    }
    
    sample <- sample[, is.na(projection)]
    
    if (ncol(sample) == 3) {
      check_installed("Ternary")
    
      Ternary::TernaryPlot(
        alab = paste(colnames(sample)[1], "\u2192"),
        blab = paste(colnames(sample)[2], "\u2192"),
        clab = paste("\u2190", colnames(sample)[3]),
        grid.lty = 'dotted',
        grid.minor.lines = 1,
        grid.minor.lty = 'dotted',
        grid.lines = 5,
        axis.labels = seq(0, 1 - sum(projection, na.rm = TRUE), length.out = 6),
        ...
      )
      Ternary::TernaryPoints(sample, pch = pch, col = cols)
      
    } else if (ncol(sample) == 2) {
      if (!oneD) {
        # diagonal plot
        if (jitter > 0) {
          j <- runif(nrow(sample),-jitter, jitter)
          sample <- sample + j
        }
        
        # line plot
        plot(
          sample,
          col = cols,
          pch = pch,
          xlim = c(0, 1),
          ylim = c(0, 1),
          ...
        )
      } else {
        Belief <- ((sample[, 1] * -1 + sample[, 2]) + 1) / 2
        
        plot(
          x = Belief,
          y = if (jitter <= 0)
            rep_len(0, nrow(sample))
          else
            abs(jitter(rep_len(0, nrow(sample)), amount = jitter))
            ,
          col = cols,
          pch = pch,
          xlim = c(0, 1),
          #ylim = c(0, 1),
          axes = FALSE,
          ylab = "",
          ...
        )
        axis(1,
          at = c(0, 1, .5),
          labels = c(colnames(sample), NA))
      }
      
    } else
      stop("projection needs to be 2d or 3d.")
    
    if (legend)
      if (is.factor(val))
        legend(
          "topright",
          legend = levels(val),
          pch = pch,
          col = col,
          bty = "n",
          title = what,
          xpd = FALSE
        ) # no clipping
    else{
      legend(
        "topright",
        legend = round(c(max(val), rep(NA, 9), min(val)), 2),
        fill = rev(col),
        border = NA,
        y.intersp = .5,
        bty = "n",
        title = what,
        xpd = FALSE
      ) # no clipping
    }
    
    invisible(list(belief = sample, val = val))
  }
