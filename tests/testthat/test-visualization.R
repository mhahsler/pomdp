test_that("belief estimates cover policy graph nodes", {
  data(Tiger)
  solution <- solve_POMDP(Tiger)

  solver_beliefs <- estimate_belief_for_nodes(solution, method = "solver_points")
  sampled_beliefs <- estimate_belief_for_nodes(
    solution,
    method = "regular_sample",
    n = 100
  )
  trajectory_beliefs <- estimate_belief_for_nodes(
    solution,
    method = "trajectories",
    n = 100
  )

  expect_equal(ncol(solver_beliefs[[1]]), length(Tiger$states))
  expect_equal(ncol(sampled_beliefs[[1]]), length(Tiger$states))
  expect_equal(ncol(trajectory_beliefs[[1]]), length(Tiger$states))
  expect_error(
    estimate_belief_for_nodes(
      solve_POMDP(Tiger, method = "enum", horizon = 2),
      method = "solver_points"
    ),
    "No solver belief points"
  )
})

test_that("base plotting helpers render to a graphics device", {
  data(Tiger)
  tiger_solution <- solve_POMDP(Tiger)
  data(Maze)
  maze_solution <- solve_MDP(Maze)

  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path), add = TRUE)
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  beliefs <- plot_belief_space(tiger_solution, n = 10, legend = FALSE)
  expect_equal(dim(beliefs$belief), c(10L, 2L))
  expect_length(beliefs$val, 10L)
  expect_invisible(plot_value_function(tiger_solution, legend = FALSE))
  expect_invisible(plot_value_function(maze_solution, legend = FALSE))
  expect_invisible(plot_transition_graph(Tiger))
  expect_no_error(
    suppressWarnings(plot_policy_graph(
      tiger_solution,
      show_belief = FALSE,
      legend = FALSE
    ))
  )
})
