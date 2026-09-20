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
