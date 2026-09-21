test_that("projection specifications are normalized and validated", {
  data(Three_doors)

  expect_named(projection(NULL, Three_doors), Three_doors$states)
  expect_equal(
    projection(c("tiger-left", "tiger-right"), Three_doors),
    c(`tiger-left` = NA, `tiger-center` = 0, `tiger-right` = NA)
  )
  expect_equal(
    projection(c(`tiger-center` = 0.2), Three_doors),
    c(`tiger-left` = NA, `tiger-center` = 0.2, `tiger-right` = NA)
  )
  expect_error(projection("unknown", Three_doors), "Unknown state")
  expect_error(
    projection(c(`tiger-left` = 0.6, `tiger-right` = 0.6), Three_doors),
    "cannot be larger than 1"
  )
})

test_that("belief-space samplers return valid probability vectors", {
  data(Tiger)
  set.seed(42)

  random <- sample_belief_space(Tiger, n = 8, method = "random")
  regular <- sample_belief_space(Tiger, n = 8, method = "regular")
  trajectories <- sample_belief_space(
    Tiger,
    n = 2,
    method = "trajectories",
    horizon = 3,
    engine = "r"
  )

  expect_equal(dim(random), c(8L, 2L))
  expect_equal(dim(regular), c(8L, 2L))
  expect_equal(rowSums(random), rep(1, 8), tolerance = 1e-7)
  expect_equal(rowSums(regular), rep(1, 8))
  expect_equal(rowSums(trajectories), rep(1, nrow(trajectories)))
  expect_error(
    sample_belief_space(Tiger, projection = "tiger-left", method = "trajectories"),
    "projection not available"
  )
})

test_that("policy helpers work for POMDP and MDP policies", {
  data(Tiger)
  tiger_solution <- solve_POMDP(Tiger)
  copied_tiger <- add_policy(Tiger, tiger_solution)

  expect_true(is_solved_POMDP(copied_tiger))
  expect_equal(policy(copied_tiger), policy(tiger_solution))
  expect_equal(regret(tiger_solution, tiger_solution), 0)
  expect_identical(as.character(optimal_action(tiger_solution, "tiger-left")), "open-right")
  expect_error(regret(Tiger, tiger_solution), "policy.*solved.*class \"POMDP\"")

  data(Maze)
  maze_solution <- solve_MDP(Maze)
  copied_maze <- add_policy(Maze, policy(maze_solution))

  expect_true(is_solved_MDP(copied_maze))
  expect_equal(policy(copied_maze), policy(maze_solution))
  expect_equal(regret(maze_solution, maze_solution, start = Maze$states[1]), 0)
  expect_error(regret(Maze, maze_solution), "policy.*solved.*class \"MDP\"")
})

test_that("action and transition helpers expose model structure", {
  data(RussianTiger)
  expect_setequal(
    actions(RussianTiger, "tiger-left"),
    c("listen", "open-left", "open-right")
  )
  expect_identical(actions(RussianTiger, "done"), "nothing")

  graph <- transition_graph(RussianTiger, simplify_transitions = FALSE)
  expect_s3_class(graph, "igraph")
  expect_setequal(igraph::V(graph)$name, RussianTiger$states)
  expect_gt(igraph::gsize(graph), 0)
})
