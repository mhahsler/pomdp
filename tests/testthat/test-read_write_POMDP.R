library("testthat")
library("pomdp")

## context("read and write_POMDP")

data(Tiger)

on.exit(file.remove("Tiger.POMDP"))
write_POMDP(Tiger, "Tiger.POMDP", labels = TRUE)
Tiger2 <- read_POMDP("Tiger.POMDP", parse = TRUE, normalize = FALSE)

fields <- c("states", "observations", "actions", "start", "discount", "horizon", 
  "transition_prob", "observation_prob", "reward")
expect_equal(Tiger[fields], Tiger2[fields])

fields <- c("transition_prob", "observation_prob", "reward")
Tiger_norm <- normalize_POMDP(Tiger, sparse = FALSE)
Tiger2_norm <- normalize_POMDP(Tiger2, sparse = FALSE)
expect_equal(Tiger_norm[fields], Tiger2_norm[fields])


# check that the solutions agree
# Note: the POMDP format does not include horizon.
sol <- solve_POMDP(Tiger)
sol2 <- solve_POMDP(Tiger2)

sol$solution$solver_output <- NULL
sol2$solution$solver_output <- NULL

expect_equal(sol$solution, sol2$solution)
