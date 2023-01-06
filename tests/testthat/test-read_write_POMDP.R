library("testthat")
library("pomdp")

## context("read and write_POMDP")

data(Tiger)

on.exit(file.remove("Tiger.POMDP"))
write_POMDP(Tiger, "Tiger.POMDP")
Tiger2 <- read_POMDP("Tiger.POMDP", parse_matrices = "dense")

fields <- c("states", "observations", "actions", "start", "discount")
expect_equal(Tiger[fields], Tiger2[fields])

fields <- c("transition_prob", "observation_prob", "reward")
Tiger_norm <- normalize_POMDP(Tiger, sparse = FALSE)
expect_equal(Tiger_norm[fields], Tiger2[fields])


# check that the solutions agree
# Note: the POMDP format does not include horizon.
sol <- solve_POMDP(Tiger)
sol2 <- solve_POMDP(Tiger2)
expect_equal(sol$solution, sol2$solution)