library("testthat")
library("pomdp")

## context("MDP_functions")

data("Maze")
m2 <- normalize_MDP(Maze, sparse = FALSE)
m3 <- normalize_MDP(Maze, sparse = TRUE)

s_abs <-c("s_5" = 5, "s_11" = 11, "s_12" = 12)
expect_equal(which(absorbing_states(Maze)), s_abs)
expect_equal(which(absorbing_states(m2)), s_abs)
expect_equal(which(absorbing_states(m3)), s_abs) 


reward_val(Maze, "up", 1, 1)
reward_val(m2, "up", 1, 1)
reward_val(m3, "up", 1, 1)

