library("testthat")
library("pomdp")

context("solve_POMDP")

data("Tiger")
sol <- solve_POMDP(Tiger)
expect_identical(nrow(sol$solution$pg), 5L)
plot(sol)

sol <- solve_POMDP(Tiger, horizon = 3, method = "incprune")
expect_identical(length(sol$solution$pg), 3L)

reward(sol)
reward(sol, belief = c(0,1))
reward(sol, belief = c(0,1), epoch = 3)

context("solve_POMDP with terminal values")

# solve 10 epochs
sol <- solve_POMDP(Tiger, discount = 1, horizon = 10, method = "enum")
alpha_horizon <- sol$solution$alpha[[1]]
pg_horizon <- sol$solution$pg[[1]]

# compare with 10 times 1 episode with the last episode as the terminal values
sol <- solve_POMDP(Tiger, discount = 1, horizon = 1, method = "enum")
for(i in 2:10)
  sol <- solve_POMDP(Tiger, discount = 1, horizon = 1, method = "enum", terminal_values = sol$solution$alpha[[1]])
alpha_stepwise <- sol$solution$alpha[[1]]
pg_stepwise <- sol$solution$pg[[1]]


expect_equal(alpha_horizon, alpha_stepwise)
expect_equal(pg_horizon$action, pg_stepwise$action) # transitions do not work


context("solve_POMDP and model files")

sol <- solve_POMDP("http://www.pomdp.org/examples/1d.POMDP")
plot(sol)

sol <- solve_POMDP("http://www.pomdp.org/examples/cheese.95.POMDP")
plot(sol)

sol <- solve_POMDP("http://www.pomdp.org/examples/shuttle.95.POMDP", parameter = list(fg_points = 10))
plot(sol)

sol <- solve_POMDP("http://www.pomdp.org/examples/stand-tiger.95.POMDP")
plot(sol)

