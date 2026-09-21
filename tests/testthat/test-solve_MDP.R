test_that("all MDP solvers return policies", {

data("Maze")

methods <- c("value_iteration", "policy_iteration", "q_learning", "sarsa",
             "expected_sarsa")

for (m in methods) {
  sol <- solve_MDP(Maze, method = m)
  pol <- policy(sol)
  expect_identical(dim(pol), c(length(Maze$states), 3L))
  
  #check_and_fix_MDP(sol)
}
})

test_that("dynamic-programming solvers report convergence contracts", {
  data("Maze")
  data("Tiger")

  expect_error(solve_MDP(Tiger), "class \"MDP\"")
  expect_error(solve_MDP_DP(Tiger), "class \"MDP\"")
  expect_error(
    solve_MDP_DP(Maze, method = "policy_iteration", horizon = 2),
    "not implemented"
  )

  expect_warning(
    unfinished <- solve_MDP_DP(
      Maze, method = "value_iteration", N_max = 1, error = 1e-12
    ),
    "did not converge"
  )
  expect_false(unfinished$solution$converged)
  expect_identical(unfinished$solution$iterations, 1L)

  finite <- expect_output(
    solve_MDP_DP(Maze, method = "value_iteration", horizon = 3, verbose = TRUE),
    "Iteration for t"
  )
  expect_length(finite$solution$policy, 3)
})

test_that("unavailable actions are excluded and never selected", {
  model <- MDP(
    states = c("start", "done"),
    actions = c("advance", "blocked"),
    transition_prob = list(advance = "identity", blocked = "identity"),
    reward = rbind(
      R_("advance", value = 1),
      R_("blocked", value = -Inf)
    ),
    discount = .5
  )

  expect_identical(actions(model, "start"), "advance")
  solved <- solve_MDP_DP(model, error = 1e-8)
  expect_true(solved$solution$converged)
  expect_true(all(as.character(policy(solved)$action) == "advance"))
})

test_that("solver initialization, diagnostics, and horizon errors are stable", {
  data("Maze")

  policy_solution <- expect_output(
    solve_MDP_DP(
      Maze, method = "policy_iteration", U = rep(0, length(Maze$states)),
      verbose = TRUE
    ),
    "Iteration"
  )
  expect_true(policy_solution$solution$converged)

  learned <- expect_output(
    solve_MDP_TD(
      Maze, method = "q_learning", horizon = 1, N = 1,
      U = rep(0, length(Maze$states)), verbose = TRUE
    ),
    "Episode 1"
  )
  expect_identical(learned$solution$N, 1)

  no_horizon <- MDP(
    states = c("left", "right"),
    actions = "move",
    transition_prob = list(move = matrix(c(0, 1, 1, 0), 2, byrow = TRUE)),
    reward = R_("move", value = 0)
  )
  no_horizon$horizon <- NULL
  expect_error(
    solve_MDP_TD(no_horizon, horizon = NULL),
    "no absorbing states"
  )
})

test_that("printing and epoch lookup distinguish finite and stationary policies", {
  data("Maze")
  expect_output(print(Maze), "Size:.*states.*actions")

  finite <- solve_MDP_DP(Maze, horizon = 2)
  expect_output(print(finite), "Solved:")
  expect_identical(pomdp:::.get_pol_index(finite, 2), 2L)
  expect_error(pomdp:::.get_pol_index(finite, 0), "positive integer")
  expect_error(pomdp:::.get_pol_index(finite, 3), "only a policy up to epoch 2")

  stationary <- solve_MDP_DP(Maze)
  expect_identical(pomdp:::.get_pol_index(stationary, 100), 1L)
})
