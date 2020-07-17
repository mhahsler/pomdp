library("testthat")
library("pomdp")

context("read and write_POMDP")

data(Tiger)
on.exit(file.remove("Tiger.POMDP"))

write_POMDP(Tiger, "Tiger.POMDP")

Tiger2 <- read_POMDP("Tiger.POMDP")


fields <- c("states", "observations", "actions", "start", "discount")
expect_equal(Tiger$model[fields], Tiger2$model[fields])

# check that the solutions agree
# Note: the POMDP format does not include horizon.
sol <- solve_POMDP(Tiger)
sol2 <- solve_POMDP(Tiger2, horizon = Inf)
expect_equal(sol$solution, sol2$solution)

# TODO: These don't work since not all fields are parsed.
#solve_POMDP(Tiger2, horizon = 3)
#transition_matrix(Tiger2)
