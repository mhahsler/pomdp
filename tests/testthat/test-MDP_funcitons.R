library("testthat")
library("pomdp")

## context("MDP_functions")

data("Maze")
m2 <- normalize_MDP(Maze, sparse = FALSE)
m3 <- normalize_MDP(Maze, sparse = TRUE)
m4 <- MDP2POMDP(Maze)

s_abs <- c("s(1,4)", "s(2,4)")
expect_equal(names(which(absorbing_states(Maze))), s_abs)
expect_equal(names(which(absorbing_states(m2))), s_abs)
expect_equal(names(which(absorbing_states(m3))), s_abs) 
expect_equal(names(which(absorbing_states(m4))), s_abs) 

v1 <- reward_val(Maze, "north", 1, 1)
v2 <- reward_val(m2, "north", 1, 1)
v3 <- reward_val(m3, "north", 1, 1)
v4 <- reward_val(m4, "north", 1, 1)

expect_equal(v1, v2)
expect_equal(v1, v3)
expect_equal(v1, v4)

