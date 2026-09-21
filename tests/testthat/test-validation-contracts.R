test_that("incomplete and malformed model specifications fail clearly", {
  transition <- list(wait = "identity")
  observation <- list(wait = "uniform")
  reward <- R_("wait", value = 0)

  expect_error(
    POMDP("s", "wait", "o", NULL, observation, reward),
    "can only miss"
  )
  expect_error(
    POMDP("s", "wait", "o", transition, NULL, reward),
    "can only miss"
  )
  expect_error(
    MDP("s", "wait", transition, NULL),
    "can only miss"
  )
  expect_error(
    MDP("s", "wait", transition, R_("wait", value = 0), discount = 0),
    "range"
  )
  expect_error(
    MDP("s", "wait", transition, R_("wait", value = 0), horizon = 1.5),
    "integer"
  )
  expect_error(
    MDP(c("s1", "s2"), "wait", transition,
        R_("wait", value = 0), start = c(.2, .2)),
    "does not add up"
  )
  expect_error(
    MDP("s", "wait", transition, R_("wait", value = 0), start = "missing"),
    "unknown or duplicate states"
  )
})

test_that("matrix, data-frame, and function contracts are validated", {
  reward <- R_("wait", value = 0)

  expect_error(
    MDP(c("s1", "s2"), "wait", list(wait = matrix(1, 1, 1)), reward),
    "right dimensions"
  )
  expect_error(
    MDP(c("s1", "s2"), "wait",
        list(wait = matrix(c(1, 0, .2, .2), 2, byrow = TRUE)), reward),
    "rows do not add up"
  )
  expect_error(
    MDP("s", c("wait", "go"), list(wait = "identity"), reward),
    "action go is missing"
  )
  expect_error(
    MDP("s", "wait", list(wait = "identity"),
        data.frame(action = "wait", value = 0)),
    "columns named"
  )
  expect_error(
    MDP("s", "wait", function(state) 1, reward),
    "formal arguments"
  )

  expect_error(
    POMDP(c("s1", "s2"), "wait", c("o1", "o2"),
      list(wait = "identity"), list(wait = matrix(1, 1, 1)), reward),
    "right dimensions"
  )
  expect_error(
    POMDP(c("s1", "s2"), "wait", c("o1", "o2"),
      list(wait = "identity"),
      list(wait = matrix(c(1, 0, .2, .2), 2, byrow = TRUE)), reward),
    "rows do not add up"
  )
  expect_error(
    POMDP("s", "wait", "o", list(wait = "identity"),
      list(wait = "uniform"), reward,
      terminal_values = matrix(1:3, nrow = 1)),
    "Terminal values"
  )
})

test_that("numeric dimensions and wildcard data frames are normalized", {
  model <- POMDP(
    states = 2,
    actions = 1,
    observations = 2,
    transition_prob = data.frame(
      action = "*", start.state = "*", end.state = "*", probability = .5
    ),
    observation_prob = data.frame(
      action = "*", end.state = "*", observation = "*", probability = .5
    ),
    reward = data.frame(
      action = "*", start.state = "*", end.state = "*",
      observation = "*", value = 0
    )
  )

  expect_identical(model$states, c("s1", "s2"))
  expect_identical(model$actions, "a1")
  expect_identical(model$observations, c("o1", "o2"))
  expect_true(all(is.na(model$transition_prob[1, 1:3])))
})

test_that("solved-model helpers enforce their public contracts", {
  data("Tiger")
  data("Maze")

  expect_false(is_solved_POMDP(Tiger))
  expect_error(is_solved_POMDP(Tiger, stop = TRUE), "solve_POMDP")
  expect_error(
    is_solved_POMDP(Maze),
    "`x` must be an object of class \"POMDP\".",
    fixed = TRUE
  )
  expect_false(is_solved_MDP(Maze))
  expect_error(is_solved_MDP(Maze, stop = TRUE), "solve_MDP")
  expect_error(
    is_solved_MDP(Tiger),
    "`x` must be an object of class \"MDP\".",
    fixed = TRUE
  )
})

test_that("model class errors identify the argument and expected class", {
  data("Tiger")
  data("Maze")

  expect_error(
    solve_POMDP(Maze),
    "`model` must be an object of class \"POMDP\".",
    fixed = TRUE
  )
  expect_error(
    solve_MDP(Tiger),
    "`model` must be an object of class \"MDP\".",
    fixed = TRUE
  )
  expect_error(
    normalize_POMDP(Maze),
    "`x` must be an object of class \"POMDP\".",
    fixed = TRUE
  )
  expect_error(
    make_partially_observable(Tiger),
    "`x` must be an object of class \"MDP\".",
    fixed = TRUE
  )

  malformed <- Maze
  malformed$reward <- matrix(0)
  expect_error(
    make_partially_observable(malformed),
    "`x$reward` must be an object of class \"data.frame\".",
    fixed = TRUE
  )
})

test_that("start beliefs support distributions, inclusion, and exclusion", {
  data("Tiger")

  model <- Tiger
  model$start <- 1L
  expect_equal(start_vector(model), c(`tiger-left` = 1, `tiger-right` = 0))

  model$start <- c(1L, 2L)
  expect_equal(start_vector(model), c(`tiger-left` = .5, `tiger-right` = .5))

  model$start <- -1L
  expect_equal(start_vector(model), c(`tiger-left` = 0, `tiger-right` = 1))

  model$start <- c("-", "tiger-left")
  expect_equal(start_vector(model), c(`tiger-left` = 0, `tiger-right` = 1))

  model$start <- matrix(c(.25, .75), nrow = 1)
  expect_equal(start_vector(model), matrix(c(.25, .75), nrow = 1,
    dimnames = list(NULL, Tiger$states)))

  model$start <- matrix(1, nrow = 1, ncol = 3)
  expect_error(start_vector(model), "one column per state")
  model$start <- "unknown"
  expect_error(start_vector(model), "unknown or duplicate states")
  model$start <- 0L
  expect_error(start_vector(model), "indices are invalid")
})

test_that("POMDP printing, convergence, and epoch lookup are explicit", {
  data("Tiger")
  expect_output(print(Tiger), "Solved: FALSE")

  solved <- solve_POMDP(Tiger, horizon = 2, method = "enum")
  expect_output(print(solved), "alpha vectors")
  expect_false(is_converged_POMDP(solved))
  expect_error(
    is_converged_POMDP(solved, stop = TRUE, message = "stationary policy required"),
    "stationary policy required"
  )
  expect_identical(pomdp:::.get_pg_index(solved, 2), 2L)
  expect_error(pomdp:::.get_pg_index(solved, 0), "positive integer")
  expect_error(pomdp:::.get_pg_index(solved, 3), "only solutions for 2 epochs")
  expect_s3_class(pomdp:::.get_pg(solved, 1), "data.frame")
  expect_true(is.matrix(pomdp:::.get_alpha(solved, 1)))
})
