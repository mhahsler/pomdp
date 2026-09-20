test_that("infinite- and finite-horizon POMDPs produce policies", {
  data(Tiger)

  infinite <- solve_POMDP(Tiger)
  expect_identical(nrow(infinite$solution$pg[[1]]), 5L)
  expect_s3_class(policy_graph(infinite), "igraph")

  finite <- solve_POMDP(Tiger, horizon = 3, method = "incprune")
  expect_length(finite$solution$pg, 3L)
  expect_true(all(is.finite(reward(finite))))
  expect_true(is.finite(reward(finite, belief = c(0, 1))))
  expect_true(is.finite(reward(finite, belief = c(0, 1), epoch = 3)))
})

test_that("terminal values compose across one-epoch solutions", {
  data(Tiger)

  direct <- solve_POMDP(Tiger, discount = 1, horizon = 10, method = "enum")
  stepwise <- solve_POMDP(Tiger, discount = 1, horizon = 1, method = "enum")
  for (i in 2:10) {
    stepwise <- solve_POMDP(
      Tiger,
      discount = 1,
      horizon = 1,
      method = "enum",
      terminal_values = stepwise$solution$alpha[[1]]
    )
  }

  expect_equal(stepwise$solution$alpha[[1]], direct$solution$alpha[[1]])
  expect_equal(
    stepwise$solution$pg[[1]]$action,
    direct$solution$pg[[1]]$action
  )
})

test_that("bundled POMDP files can be solved without network access", {
  path <- system.file("examples/shuttle_95.POMDP", package = "pomdp")
  solution <- solve_POMDP(path, parameter = list(fg_points = 10))

  expect_s3_class(policy_graph(solution), "igraph")
  expect_s3_class(policy(solution), "data.frame")
})

test_that("terminal rewards affect finite-horizon solutions", {
  data(Tiger)
  set.seed(42)

  exact <- solve_POMDP(
    Tiger,
    horizon = 3,
    discount = 1,
    method = "incprune",
    terminal_values = c(0, 1000)
  )
  expect_gt(reward(exact), 100)
  expect_gt(simulate_POMDP(exact, n = 100)$avg_reward, 100)
  expect_gt(simulate_POMDP(exact, engine = "r", n = 100)$avg_reward, 100)

  expect_warning(
    approximate <- solve_POMDP(
      Tiger,
      horizon = 3,
      discount = 1,
      method = "grid",
      terminal_values = c(0, 1000)
    ),
    "may not be valid"
  )
  expect_warning(reward_node_action(approximate), "may not be valid")
  expect_gt(suppressWarnings(reward(approximate)), 100)
  expect_gt(simulate_POMDP(approximate, n = 100)$avg_reward, 100)
  expect_gt(
    simulate_POMDP(approximate, engine = "r", n = 100)$avg_reward,
    100
  )
})
