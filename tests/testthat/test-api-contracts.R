test_that("model labels and horizons have consistent validation", {
  transition <- list(wait = "identity")
  reward <- R_("wait", value = 0)

  expect_error(MDP(character(), "wait", transition, reward), "states must be")
  expect_error(MDP(c("s", "s"), "wait", transition, reward), "states must contain unique")
  expect_error(MDP("s", c("wait", "wait"), transition, reward), "actions must contain unique")
  expect_error(MDP(NA_character_, "wait", transition, reward), "states must be")
  expect_error(MDP("s", "wait", transition, reward, horizon = 0), "positive integer")
  expect_error(MDP("s", "wait", transition, reward, horizon = -1), "positive integer")
  expect_error(MDP("s", "wait", transition, reward, horizon = NA), "positive integer")
  expect_error(MDP(-1, "wait", transition, reward), "states must be")
  expect_error(MDP("s", 0, transition, reward), "actions must be")
  expect_error(MDP("s", "wait", transition, reward, discount = NA), "single finite")

  expect_error(
    POMDP("s", "wait", character(), transition, list(wait = "uniform"), reward),
    "observations must be"
  )
})

test_that("accessors reject unknown and ambiguous selectors", {
  data("Tiger")

  expect_error(transition_matrix(Tiger, action = "unknown"), "Unknown action")
  expect_error(transition_matrix(Tiger, action = 0), "action index")
  expect_error(transition_matrix(Tiger, action = "listen", start.state = "unknown"),
    "Unknown start.state")
  expect_error(observation_matrix(Tiger, action = "listen", observation = "unknown"),
    "Unknown observation")
  expect_error(reward_matrix(Tiger, action = "listen", end.state = "tiger-left"),
    "start.state needs")
  expect_error(transition_matrix(Tiger, start.state = "tiger-left"),
    "action needs")
  expect_error(transition_matrix(Tiger, episode = 1, epoch = 1),
    "only one")
  expect_error(transition_matrix(Tiger, episode = 2), "between 1 and 1")
  expect_error(transition_matrix(Tiger, epoch = 0), "positive integer")
  expect_error(transition_matrix(Tiger, drop = NA), "TRUE or FALSE")
})

test_that("drop controls accessor return shapes across representations", {
  data("Tiger")
  base_model <- Tiger
  functional <- Tiger
  functional$transition_prob <- function(action, start.state, end.state) {
    transition_matrix(base_model, action, start.state, end.state)
  }
  functional$observation_prob <- function(action, end.state, observation) {
    observation_matrix(base_model, action, end.state, observation)
  }
  functional$reward <- function(action, start.state, end.state, observation) {
    reward_matrix(base_model, action, start.state, end.state, observation)
  }
  models <- list(
    original = Tiger,
    dense = normalize_POMDP(Tiger, sparse = FALSE),
    sparse = normalize_POMDP(Tiger, sparse = TRUE),
    functional = functional
  )

  for (model in models) {
    transition <- transition_matrix(
      model, "listen", "tiger-left", "tiger-left", drop = FALSE
    )
    expect_equal(dim(transition), c(1L, 1L))
    expect_equal(dimnames(transition), list("tiger-left", "tiger-left"))
    expect_equal(
      transition_matrix(model, "listen", "tiger-left", "tiger-left"),
      1
    )

    transition_row <- transition_matrix(
      model, "listen", "tiger-left", drop = FALSE
    )
    expect_equal(dim(transition_row), c(1L, 2L))

    observation <- observation_matrix(
      model, "listen", "tiger-left", "tiger-left", drop = FALSE
    )
    expect_equal(dim(observation), c(1L, 1L))

    reward <- reward_matrix(
      model, "listen", "tiger-left", "tiger-left", "tiger-left",
      drop = FALSE
    )
    expect_equal(dim(reward), c(1L, 1L))
    expect_equal(drop(reward), -1)
  }
})

test_that("drop is consistent for policy and value functions", {
  data("Maze")
  solved <- solve_MDP(Maze)

  expect_s3_class(policy(solved, drop = TRUE), "data.frame")
  expect_type(policy(solved, drop = FALSE), "list")
  expect_true(is.numeric(value_function(solved, drop = TRUE)))
  expect_type(value_function(solved, drop = FALSE), "list")
  expect_error(policy(solved, drop = NA), "TRUE or FALSE")
  expect_error(value_function(solved, drop = 1), "TRUE or FALSE")
})

test_that("belief validation handles vectors, selections, and matrices", {
  data("Tiger")
  model <- Tiger

  model$start <- c(.25, .75)
  expect_equal(start_vector(model), c(.25, .75))
  model$start <- c(`tiger-right` = .75, `tiger-left` = .25)
  expect_equal(start_vector(model), c(`tiger-left` = .25, `tiger-right` = .75))
  model$start <- c(.2, .2)
  expect_error(start_vector(model), "add up to 1")
  model$start <- c("tiger-left", "tiger-left")
  expect_error(start_vector(model), "duplicate")
  model$start <- c(-1, 2)
  expect_error(start_vector(model), "indices are invalid")
  model$start <- matrix(c(.8, .2, .3, .3), nrow = 2, byrow = TRUE)
  expect_error(start_vector(model), "probability rows")
})

test_that("state, action, and observation helpers validate model labels", {
  data("Tiger")
  data("RussianTiger")

  expect_error(actions(RussianTiger, "unknown"), "Unknown state")
  expect_error(actions(RussianTiger, c("done", "tiger-left")), "exactly one")
  expect_error(update_belief(Tiger, action = "unknown"), "Unknown action")
  expect_error(update_belief(Tiger, observation = "unknown"), "Unknown observation")
  expect_error(update_belief(Tiger, drop = NA), "TRUE or FALSE")

  updated <- update_belief(
    Tiger, action = "listen", observation = "tiger-left", drop = FALSE
  )
  expect_equal(dim(updated), c(1L, 2L))
})

test_that("simulation return containers do not depend on the engine", {
  data("Tiger")

  set.seed(5)
  r <- simulate_POMDP(Tiger, n = 2, horizon = 2, engine = "r")
  set.seed(5)
  cpp <- simulate_POMDP(Tiger, n = 2, horizon = 2, engine = "cpp")

  expect_s3_class(r$trajectories, "data.frame")
  expect_s3_class(cpp$trajectories, "data.frame")
  expect_equal(dim(r$trajectories), c(0L, 0L))
  expect_equal(dim(cpp$trajectories), c(0L, 0L))
  expect_equal(dim(r$belief_states), c(0L, 0L))
  expect_equal(dim(cpp$belief_states), c(0L, 0L))

  expect_error(simulate_POMDP(Tiger, n = 0, horizon = 2), "positive integer")
  expect_error(simulate_POMDP(Tiger, n = 1, horizon = 1.5), "positive integer")
})
