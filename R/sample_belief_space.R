#' Sample from the Belief Space
#'
#' Sample points from belief space using a several sampling strategies.
#'
#' The purpose of sampling from the belief space is to provide good coverage or to sample belief points
#' that are more likely to be encountered (see trajectory method).
#' The following sampling methods are available:
#' 
#' * `'random'` samples uniformly sample from the projected belief space using
#' the method described by Luc Devroye (1986). 
#' 
#' * `'regular'` samples points using a
#' regularly spaced grid. This method is only available for projections on 2 or
#' 3 states.  
#' 
#' * `'vertices'` only samples from the vertices of the belief space.
#'
#' * `"trajectories"` returns the belief states encountered in `n` trajectories of length `horizon` starting at the
#' model's initial belief. Thus it returns `n` x `horizon` belief states and will contain duplicates. 
#' Projection is not supported for trajectories. Additional
#' arguments can include the simulation `horizon` and the start `belief` which are passed on to [simulate_POMDP()].
#'
#' @family POMDP
#' 
#' @param model a unsolved or solved [POMDP].
#' @param projection Sample in a projected belief space. All states not
#' included in the projection are held at a belief of 0. `NULL` means no
#' projection.
#' @param n size of the sample. For trajectories, it is the number of trajectories.
#' @param method character string specifying the sampling strategy. Available
#' are `"random"`, `"regular"`, `"vertices"`, and `"trajectories"`.
#' @param ... for the trajectory method, further arguments are passed on to [simulate_POMDP()]. Further arguments are ignored for the other methods. 
#' @return Returns a matrix. Each row is a sample from the belief space.
#' @author Michael Hahsler
#' @references Luc Devroye, Non-Uniform Random Variate Generation, Springer
#' Verlag, 1986.
#' @examples
#' data("Tiger")
#'
#' sample_belief_space(Tiger, n = 5)
#' sample_belief_space(Tiger, n = 5, method = "regular")
#' sample_belief_space(Tiger, n = 5, horizon = 5, method = "trajectories") 
#'
#' # sample and calculate the reward for a solved POMDP
#' sol <- solve_POMDP(Tiger)
#' samp <- sample_belief_space(sol, n = 5, method = "regular")
#' rew <- reward(sol, belief = samp)
#' cbind(samp, rew)
#' @export
sample_belief_space <-
  function(model,
    projection = NULL,
    n = 1000,
    method = "random", ...) {
    method <-
      match.arg(method, choices = c("random", "regular", "vertices", "trajectories"))
    
    # check 
    if(method == "trajectories" && !is.null(projection)) stop("projection is not supported for method 'trajectories'.")
    
    if (is.null(projection))
      projection <- seq(length(model$states))
    if (is.character(projection))
      projection <- pmatch(projection, model$states)
    d <- length(projection)
    if (d < 2)
      stop("Projection needs to be on 2 or more states.")
    
    if (any(duplicated(projection)) || any(is.na(match(projection, seq_along(model$states)))))
      stop("illegal specification for projection.")
    
    # empty belief states

    
    belief_states <- switch(method,
      random = {
        # uniformly sample from a simplex.
        # https://cs.stackexchange.com/questions/3227/uniform-sampling-from-a-simplex)
        # Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag, 1986.
        belief_states <-
          matrix(0, nrow = n, ncol = length(model$states))
        colnames(belief_states) <- model$states
        
        m <-
          cbind(0, matrix(stats::runif(n * (d - 1)), ncol = d - 1), 1)
        belief_states[, projection] <-
          t(apply(
            m,
            MARGIN = 1,
            FUN = function(x)
              diff(sort(x))
          ))
        belief_states
      }, 
      
      regular = {
        belief_states <-
          matrix(0, nrow = n, ncol = length(model$states))
        colnames(belief_states) <- model$states
        
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
                ncol = length(model$states))
            colnames(belief_states) <- model$states
          }
          
          belief_states[, projection] <-
            t(Ternary::XYToTernary(triangleCentres["x", ], triangleCentres["y", ],
              direction = 1))
          attr(belief_states, "TernaryTriangleCenters") <-
            triangleCentres
        } else
          stop("method regular is only available for projections on 2 or 3 states.")
        
        belief_states
      },
      
      vertices = {
        belief_states <-
          matrix(0, nrow = n, ncol = length(model$states))
        colnames(belief_states) <- model$states
        
        belief_states <- belief_states[rep(1, d),]
        belief_states[cbind(projection, projection)] <- 1
        belief_states <-
          belief_states[sample(d, size = n, replace = TRUE),]
        
        belief_states
      },
      
      trajectories = {
        simulate_POMDP(model, n = n, visited_beliefs = TRUE, ...) 
      }
      
      )
    
    belief_states
  }
