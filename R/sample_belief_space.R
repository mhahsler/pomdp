#' Sample from the Belief Space
#'
#' Sample randomly (uniform) or regularly spaced points from the projected
#' belief space.
#'
#' Method random samples uniformly sample from the projected belief space using
#' the method described by Luc Devroye. Method regular samples points using a
#' regularly spaced grid. This method is only available for projections on 2 or
#' 3 states.  Method vertices only samples from the vertices of the belief
#' space.
#'
#' @param model a unsolved or solved POMDP.
#' @param projection Sample in a projected belief space. All states not
#' included in the projection are held at a belief of 0. \code{NULL} means no
#' projection.
#' @param n size of the sample.
#' @param method character string specifying the sampling strategy. Available
#' are \code{"random"}, \code{"regular"}, and \code{"vertices"}.
#' @return Returns a matrix. Each row is a sample from the belief space.
#' @author Michael Hahsler
#' @references Luc Devroye, Non-Uniform Random Variate Generation, Springer
#' Verlag, 1986.
#' @examples
#'
#' data("Tiger")
#'
#' sample_belief_space(Tiger, n = 5)
#' sample_belief_space(Tiger, n = 5, method = "regular")
#'
#' # sample and calculate the reward for a solve POMDP
#' sol <- solve_POMDP(Tiger)
#' reward(sol, belief = sample_belief_space(sol, n = 5, method = "regular"))
#'
#' @export
sample_belief_space <-
  function(model,
    projection = NULL,
    n = 1000,
    method = "random") {
    method <-
      match.arg(method, choices = c("random", "regular", "vertices"))
    
    if (is.null(projection))
      projection <- seq(length(model$model$states))
    if (is.character(projection))
      projection <- pmatch(projection, model$model$states)
    d <- length(projection)
    if (d < 2)
      stop("Projection needs to be on 2 or more states.")
    
    # empty belief states
    belief_states <-
      matrix(0, nrow = n, ncol = length(model$model$states))
    colnames(belief_states) <- model$model$states
    
    switch(method,
      random = {
        # uniformly sample from a simplex.
        # https://cs.stackexchange.com/questions/3227/uniform-sampling-from-a-simplex)
        # Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986.
        m <- cbind(0, matrix(stats::runif(n * (d - 1)), ncol = d - 1), 1)
        belief_states[, projection] <-
          t(apply(
            m,
            MARGIN = 1,
            FUN = function(x)
              diff(sort(x))
          ))
      },
      
      regular = {
        if (d == 2) {
          b <- seq(0, 1, length.out = n)
          belief_states[, projection] <- cbind(b, 1 - b)
        } else if (d == 3) {
          
          check_installed("Ternary")
          
          ### Note: the number of points might not be exactly n!
          triangleCentres <-
            Ternary::TriangleCentres(round(n ^ .5), direction = 1)
          if (ncol(triangleCentres) != n) {
            warning(
              "Regular triangle grid produces ",
              ncol(triangleCentres),
              " instead of ",
              n,
              " belief space samples!"
            )
            belief_states <-
              matrix(0,
                nrow = ncol(triangleCentres),
                ncol = length(model$model$states))
            colnames(belief_states) <- model$model$states
          }
          
          belief_states[, projection] <-
            t(Ternary::XYToTernary(triangleCentres["x",], triangleCentres["y",],
              direction = 1))
          attr(belief_states, "TernaryTriangleCenters") <-
            triangleCentres
        } else
          stop("method redular is only available for projections on 2 or 3 states.")
      },
      
      vertices = {
        belief_states <- belief_states[rep(1, d), ]
        belief_states[cbind(projection, projection)] <- 1
        belief_states <-
          belief_states[sample(d, size = n, replace = TRUE), ]
      })
    
    belief_states
  }
