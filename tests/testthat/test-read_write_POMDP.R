test_that("POMDP files round-trip without changing the model", {
  data(Tiger)
  path <- tempfile(fileext = ".POMDP")
  on.exit(unlink(path), add = TRUE)

  write_POMDP(Tiger, path, labels = TRUE)
  restored <- read_POMDP(path, parse = TRUE, normalize = FALSE)

  fields <- c(
    "states", "observations", "actions", "start", "discount", "horizon",
    "transition_prob", "observation_prob", "reward"
  )
  expect_equal(restored[fields], Tiger[fields])

  matrix_fields <- c("transition_prob", "observation_prob", "reward")
  expect_equal(
    normalize_POMDP(restored, sparse = FALSE)[matrix_fields],
    normalize_POMDP(Tiger, sparse = FALSE)[matrix_fields]
  )
})

test_that("reader accepts matrix and keyword representations", {
  path <- tempfile(fileext = ".POMDP")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "discount: 0.9",
    "values: reward",
    "states: s0 s1",
    "actions: stay switch",
    "observations: left right",
    "start: 1 0",
    "T: stay", "identity",
    "T: switch", "0 1", "1 0",
    "O: *", "uniform",
    "R: * : * : * : * 0"
  ), path)

  model <- read_POMDP(path, parse = TRUE, normalize = FALSE)
  expect_equal(unname(transition_matrix(model, "stay")), diag(2))
  expect_equal(
    unname(transition_matrix(model, "switch")),
    matrix(c(0, 1, 1, 0), 2, byrow = TRUE)
  )
  expect_equal(unname(observation_matrix(model, "stay")), matrix(.5, 2, 2))
  rewards <- reward_matrix(model, "stay")
  expect_true(all(vapply(rewards, function(x) all(x == 0), logical(1))))
  expect_true(all(vapply(rewards, function(x) identical(dim(x), c(2L, 2L)), logical(1))))
})

test_that("malformed POMDP files fail with stable errors", {
  duplicate <- tempfile(fileext = ".POMDP")
  invalid_start <- tempfile(fileext = ".POMDP")
  on.exit(unlink(c(duplicate, invalid_start)), add = TRUE)

  writeLines(c(
    "discount: 0.9", "discount: 0.8", "values: reward",
    "states: s", "actions: wait", "observations: seen"
  ), duplicate)
  expect_error(read_POMDP(duplicate), "Multiple definitions.*discount")

  writeLines(c(
    "discount: 0", "values: reward",
    "states: s0 s1", "actions: wait", "observations: seen",
    "start: 0.5 0.5",
    "T: wait", "identity",
    "O: wait", "uniform",
    "R: * : * : * : * 0"
  ), invalid_start)
  expect_error(read_POMDP(invalid_start, parse = TRUE), "discount must be")
})

test_that("matrix parser supports scalar, row, column, and full-matrix forms", {
  actions <- "a"
  states <- c("s1", "s2")
  observations <- c("o1", "o2")

  scalar <- pomdp:::parse_POMDP_matrix(
    c(
      "T: a : * : * 0.5",
      "T: a : * : s1 0.2",
      "T: a : s1 : * 0.3",
      "T: a : s1 : s2 0.4"
    ),
    "T", actions, states, observations, sparse = FALSE
  )
  expect_equal(unname(scalar$a), matrix(c(.3, .4, .2, .5), 2, byrow = TRUE))

  next_line <- pomdp:::parse_POMDP_matrix(
    c(
      "T: a : * : s1", "0.2",
      "T: a : s1 : *", "0.3 0.7",
      "T: a : s2 : s2", "0.8"
    ),
    "T", actions, states, observations, sparse = FALSE
  )
  expect_equal(unname(next_line$a), matrix(c(.3, .7, .2, .8), 2, byrow = TRUE))

  row_form <- pomdp:::parse_POMDP_matrix(
    c("T: a : s1", "0.1 0.9"),
    "T", actions, states, observations, sparse = FALSE
  )
  expect_equal(unname(row_form$a[1, ]), c(.1, .9))

  full <- expect_output(
    pomdp:::parse_POMDP_matrix(
      c("T: a", "0.1 0.9", "0.8 0.2"),
      "T", actions, states, observations, sparse = FALSE, verbose = TRUE
    ),
    "Processing T"
  )
  expect_equal(unname(full$a), matrix(c(.1, .9, .8, .2), 2, byrow = TRUE))
})

test_that("reward parser supports every POMDP matrix shorthand", {
  args <- list(actions = "a", states = c("s1", "s2"),
    observations = c("o1", "o2"), sparse = FALSE)

  scalar <- do.call(pomdp:::.parse_POMDP_reward, c(list(
    problem = c("R: a : s1 : s2 : o1 7")
  ), args))
  expect_equal(scalar$a$s1["s2", "o1"], 7)

  next_line <- expect_output(
    do.call(pomdp:::.parse_POMDP_reward, c(list(
      problem = c("R: a : s1 : s2 : o1", "8"), verbose = TRUE
    ), args)),
    "Processing"
  )
  expect_equal(next_line$a$s1["s2", "o1"], 8)

  observations <- do.call(pomdp:::.parse_POMDP_reward, c(list(
    problem = c("R: a : s1 : s2", "3 4")
  ), args))
  expect_equal(unname(observations$a$s1["s2", ]), c(3, 4))

  matrix_form <- do.call(pomdp:::.parse_POMDP_reward, c(list(
    problem = c("R: a : s1", "1 2", "3 4")
  ), args))
  expect_equal(unname(matrix_form$a$s1), matrix(1:4, 2, byrow = TRUE))
})

test_that("POMDP number formatting rejects missing and nonnumeric fields", {
  expect_error(pomdp:::.format_number_fixed(NULL, debug = "reward"), "missing field")
  expect_error(pomdp:::.format_number_fixed("one"), "expects numbers")
  expect_error(pomdp:::.format_number_fixed(data.frame(x = 1)), "not implemented")
  expect_identical(pomdp:::.format_number_fixed(c(.5, 1), digits = 1), "0.5 1.0")
})

test_that("round-tripped POMDP files produce the same solution", {
  data(Tiger)
  path <- tempfile(fileext = ".POMDP")
  on.exit(unlink(path), add = TRUE)
  write_POMDP(Tiger, path, labels = TRUE)
  restored <- read_POMDP(path, parse = TRUE, normalize = FALSE)

  original_solution <- solve_POMDP(Tiger)$solution
  restored_solution <- solve_POMDP(restored)$solution
  original_solution$solver_output <- NULL
  restored_solution$solver_output <- NULL

  expect_equal(restored_solution, original_solution)
})
