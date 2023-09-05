#' Sample from the Belief Space
#'
#' Sample points from belief space using a several sampling strategies.
#'
#' The purpose of sampling from the belief space is to provide good coverage or to sample belief points
#' that are more likely to be encountered (see trajectory method).
#' The following sampling methods are available:
#'
#' * `'random'` samples uniformly sample from the projected belief space using
#' the method described by Luc Devroye (1986). Sampling is be done in parallel
#' after a foreach backend is registered.
#'
#' * `'regular'` samples points using a
#' regularly spaced grid. This method is only available for projections on 2 or
#' 3 states.
#'
#' * `"trajectories"` returns the belief states encountered in `n` trajectories of length `horizon` starting at the
#' model's initial belief. Thus it returns `n` x `horizon` belief states and will contain duplicates.
#' Projection is not supported for trajectories. Additional
#' arguments can include the simulation `horizon` and the start `belief` which are passed on to [simulate_POMDP()].
#'
#' @family POMDP
#'
#' @param model a unsolved or solved [POMDP].
#' @param projection Sample in a projected belief space. See [projection()] for details.
#' @param n size of the sample. For trajectories, it is the number of trajectories.
#' @param method character string specifying the sampling strategy. Available
#' are `"random"`, `"regular"`, and `"trajectories"`.
#' @param ... for the trajectory method, further arguments are passed on to [simulate_POMDP()]. Further arguments are ignored for the other methods.
#' @return Returns a matrix. Each row is a sample from the belief space.
#' @author Michael Hahsler
#' @references Luc Devroye, Non-Uniform Random Variate Generation, Springer
#' Verlag, 1986.
#' @examples
#' data("Tiger")
#'
#' # random sampling can be done in parallel after registering a backend.
#' # doparallel::registerDoParallel()
#'
#' sample_belief_space(Tiger, n = 5)
#' sample_belief_space(Tiger, n = 5, method = "regular")
#' sample_belief_space(Tiger, n = 1, horizon = 5, method = "trajectories")
#'
#' # sample, determine the optimal action and calculate the expected reward for a solved POMDP
#' # Note: check.names = FALSE is used to preserve the `-` for the state names in the dataframe.
#' sol <- solve_POMDP(Tiger)
#' samp <- sample_belief_space(sol, n = 5, method = "regular")
#' data.frame(samp, action = optimal_action(sol,  belief = samp), 
#'   reward = reward(sol, belief = samp), check.names = FALSE)
#'   
#' # sample from a 3 state problem
#' data(Three_doors)
#' Three_doors
#' 
#' sample_belief_space(Three_doors, n = 5)
#' sample_belief_space(Three_doors, n = 5, projection = c(`tiger-left` = .1))
#' 
#' if ("Ternary" %in% installed.packages()) {
#' sample_belief_space(Three_doors, n = 9, method = "regular")
#' sample_belief_space(Three_doors, n = 9, method = "regular", projection = c(`tiger-left` = .1))
#' }
#' 
#' sample_belief_space(Three_doors, n = 1, horizon = 5, method = "trajectories")
#' @export
sample_belief_space <-
  function(model,
    projection = NULL,
    n = 1000,
    method = "random",
    ...) {
    method <-
      match.arg(method,
        choices = c("random", "regular", "trajectories"))
    
    projection <- projection(projection, model)
     
    belief_points <- switch(
      method,
      random = {
        
        ns <- foreach_split(n)
        w <-
          NULL # to shut up the warning for the foreach counter variable
        
        foreach(w = 1:length(ns), .combine = rbind) %dopar%
          sample_simplex_cpp(ns[w], model$states, projection)
        
              # uniformly sample from a simplex. R implementation (minus new projection format) 
              # https://cs.stackexchange.com/questions/3227/uniform-sampling-from-a-simplex)
              # Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986.
              # 
              # m <-
              #   cbind(0, matrix(stats::runif(n * (d - 1)), ncol = d - 1), 1)
              # belief_states[, projection] <-
              #   t(apply(
              #     m,
              #     MARGIN = 1,
              #     FUN = function(x)
              #       diff(sort(x))
              #   ))
      },
      
      regular = {
        
        projection_sum <- 1 - sum(projection, na.rm = TRUE)
        sample_states <- which(is.na(projection))
        
        if (length(sample_states) == 2L) {
          belief_points <-
            matrix(0, nrow = n, ncol = length(model$states))
          b <- seq(0, projection_sum, length.out = n)
          belief_points[, sample_states] <- cbind(b, projection_sum - b)
           
        } else if (length(sample_states) == 3L) {
          check_installed("Ternary")
          
          ### Note: the number of points might not be exactly n!
          triangleCentres <-
            Ternary::TriangleCentres(ceiling(n ^ .5), direction = 1)
          if (ncol(triangleCentres) != n)
            warning(
              "Regular triangle grid produces ",
              ncol(triangleCentres),
              " instead of ",
              n,
              " belief space samples!"
            )
            
          belief_points <-
              matrix(0,
                nrow = ncol(triangleCentres),
                ncol = length(model$states))
          
          belief_points[, sample_states] <-
            t(Ternary::XYToTernary(triangleCentres["x",], triangleCentres["y",],
              direction = 1)) * projection_sum
          attr(belief_points, "TernaryTriangleCenters") <-
            triangleCentres
        
        } else
          stop("method regular is only available for projections on 2 or 3 states.")
        
        for (i in which(!is.na(projection)))
          belief_points[, i] <- projection[i]
        
        colnames(belief_points) <- model$states
        belief_points
      },

# TODO: We need to identify the vertices!      
# * `'vertices'` does not samples, but returns all vertices of the belief space.
#       vertices = {
#         if(any(!is.na(projection)))
#           stop("vertices cannot be used with projection!")
#         
#         belief_points <-
#           diag(length(model$states))
#         
#         colnames(belief_points) <- model$states
#         belief_points
#       },
#       
      trajectories = {
        if (!all(is.na(projection)))
          stop("projection not available for method 'trajectories'!")
        
        simulate_POMDP(model, n = n, return_beliefs = TRUE, ...)$belief_states
      }
      
    )
    
    belief_points
  }
