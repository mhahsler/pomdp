test_that("gridworld coordinates and transitions are consistent", {
  grid <- gridworld_init(
    c(2, 3),
    unreachable_states = "s(1,2)",
    absorbing_states = "s(2,3)"
  )

  expect_equal(gridworld_s2rc("s(2,3)"), c(2L, 3L))
  expect_identical(gridworld_rc2s(c(2, 3)), "s(2,3)")
  expect_error(gridworld_s2rc("bad"), "Malformed gridworld state label")
  expect_equal(grid$transition_prob("right", "s(1,1)", "s(1,1)"), 1)
  expect_equal(grid$transition_prob("up", "s(2,3)", "s(2,3)"), 1)
})

test_that("gridworld models expose state, value, and policy matrices", {
  maze <- gridworld_maze_MDP(
    dim = c(2, 3),
    start = "s(1,1)",
    goal = "s(2,3)",
    walls = "s(1,2)",
    step_cost = 0.1,
    name = "small maze"
  )
  solved <- solve_MDP(maze)

  expect_equal(dim(gridworld_matrix(maze, what = "states")), c(2L, 3L))
  expect_equal(dim(gridworld_matrix(maze, what = "labels")), c(2L, 3L))
  expect_equal(dim(gridworld_matrix(solved, what = "values")), c(2L, 3L))
  expect_equal(dim(gridworld_matrix(solved, what = "actions")), c(2L, 3L))
  expect_equal(dim(gridworld_matrix(maze, what = "absorbing")), c(2L, 3L))
  expect_equal(dim(gridworld_matrix(maze, what = "reachable")), c(2L, 3L))
})

test_that("unreachable states can be removed from normalized models", {
  model <- MDP(
    states = c("a", "b", "c"),
    actions = "move",
    transition_prob = list(move = rbind(
      c(0, 1, 0),
      c(0, 1, 0),
      c(0, 0, 1)
    )),
    reward = R_(value = 0),
    start = "b"
  )

  expect_equal(reachable_states(model), c(a = FALSE, b = TRUE, c = FALSE))
  reduced <- remove_unreachable_states(model)
  expect_identical(reduced$states, "b")
  expect_equal(dim(transition_matrix(reduced)[[1]]), c(1L, 1L))
})
