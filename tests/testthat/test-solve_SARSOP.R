library("testthat")
library("pomdp")

skip_if_not_installed("sarsop")
context("solve_SARSOP")

### In case sarsop is not installed correctly (taken from the example in ? pomdpsol)
if(sarsop::assert_has_appl()) {

  data("Tiger")
  sol_SARSOP <- solve_SARSOP(Tiger)
  sol_POMDP <- solve_POMDP(Tiger)
  
  # sort policy by first alpha vector
  normalize_policy <- function(x) {
    if(!is.data.frame(x)) x <- x[[1]]
    x <- x[order(x[,1]),]
    x
  }
  
  pol_SARSOP <- normalize_policy(policy(sol_SARSOP))
  pol_POMDP <- normalize_policy(policy(sol_POMDP))
  
  # check actions
  expect_identical(pol_SARSOP$action, pol_POMDP$action)
  
  # check alpha vectors
  expect_true(all((pol_SARSOP[,1:2] - pol_POMDP[,1:2]) < 1e-3))

}