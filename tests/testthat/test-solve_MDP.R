library("testthat")
library("pomdp")

## context("solve_MDP")

data("Maze")

methods <- c("value_iteration", "policy_iteration", "q_learning", "sarsa",
             "expected_sarsa")

for (m in methods) {
  sol <- solve_MDP(Maze, method = m)
  pol <- policy(sol)
  expect_identical(dim(pol), c(length(Maze$states), 3L))
  
  #check_and_fix_MDP(sol)
}

