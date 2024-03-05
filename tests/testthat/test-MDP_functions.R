library("testthat")
library("pomdp")

## context("MDP_functions")

data("Maze")
m2 <- normalize_MDP(Maze, sparse = FALSE)
m3 <- normalize_MDP(Maze, sparse = TRUE)
m4 <- make_partially_observable(Maze)

s_abs <- c("s(1,4)", "s(2,4)")
expect_equal(names(which(absorbing_states(Maze))), s_abs)
expect_equal(names(which(absorbing_states(m2))), s_abs)
expect_equal(names(which(absorbing_states(m3))), s_abs) 
expect_equal(names(which(absorbing_states(m4))), s_abs) 

v1 <- reward_matrix(Maze, "up", 1, 1)
v2 <- reward_matrix(m2, "up", 1, 1)
v3 <- reward_matrix(m3, "up", 1, 1)
v4 <- reward_matrix(m4, "up", 1, 1)

expect_equal(v1, unname(v2))
expect_equal(v1, v3)
expect_true(all(v4 == v1))


expect_equal(make_fully_observable(make_partially_observable(Maze)), Maze)
