library("testthat")
library("pomdp")

context("solve_POMDP")

data("TigerProblem")
sol <- solve_POMDP(TigerProblem)
expect_identical(nrow(sol$solution$pg), 5L)
plot(sol)

sol <- solve_POMDP(TigerProblem, horizon = 3, method = "incprune")
expect_identical(length(sol$solution$pg), 3L)

reward(sol)
reward(sol, belief = c(0,1))
reward(sol, belief = c(0,1), epoch = 3)


context("solve_POMDP and model files")

sol <- solve_POMDP("http://www.pomdp.org/examples/1d.POMDP")
plot(sol)

sol <- solve_POMDP("http://www.pomdp.org/examples/cheese.95.POMDP")
plot(sol)

sol <- solve_POMDP("http://www.pomdp.org/examples/shuttle.95.POMDP", parameter = list(fg_points = 10))
plot(sol)

sol <- solve_POMDP("http://www.pomdp.org/examples/stand-tiger.95.POMDP")
plot(sol)

