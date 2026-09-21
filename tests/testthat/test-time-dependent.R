make_time_dependent_tiger <- function() {
  data("Tiger", envir = environment())
  POMDP(
    name = "two-episode tiger",
    states = Tiger$states,
    actions = Tiger$actions,
    observations = Tiger$observations,
    discount = 1,
    horizon = c(normal = 1, scared = 1),
    start = Tiger$start,
    transition_prob = list(
      normal = Tiger$transition_prob,
      scared = list(
        listen = "identity",
        `open-left` = rbind(c(0, 1), c(0, 1)),
        `open-right` = rbind(c(1, 0), c(1, 0))
      )
    ),
    observation_prob = list(
      normal = Tiger$observation_prob,
      scared = Tiger$observation_prob
    ),
    reward = Tiger$reward
  )
}

test_that("epochs select the correct episode-specific dynamics", {
  model <- make_time_dependent_tiger()

  expect_true(is_timedependent_POMDP(model))
  expect_identical(epoch_to_episode(model, NULL), 1L)
  expect_identical(epoch_to_episode(model, 1), 1L)
  expect_identical(epoch_to_episode(model, 2), 2L)
  expect_error(epoch_to_episode(model, 3), "Epoch does not exist")

  normal <- transition_matrix(model, action = "open-left", episode = 1)
  scared <- transition_matrix(model, action = "open-left", episode = 2)
  expect_false(isTRUE(all.equal(normal, scared)))
  expect_equal(unname(rowSums(scared)), c(1, 1))
  expect_equal(
    observation_matrix(model, action = "listen", episode = 1),
    observation_matrix(model, action = "listen", episode = 2)
  )
  expect_equal(
    reward_matrix(model, action = "listen", episode = 1),
    reward_matrix(model, action = "listen", episode = 2)
  )
})

test_that("multi-episode models solve, normalize, and simulate", {
  model <- make_time_dependent_tiger()
  dense <- normalize_POMDP(model, sparse = FALSE)
  sparse <- normalize_POMDP(model, sparse = TRUE)

  expect_true(is_timedependent_POMDP(dense))
  expect_true(is_timedependent_POMDP(sparse))
  expect_length(dense$transition_prob, 2)
  expect_length(sparse$observation_prob, 2)

  solved <- solve_POMDP(model, method = "enum")
  expect_true(is_solved_POMDP(solved))
  expect_length(solved$solution$alpha, 2)

  set.seed(2026)
  sim_r <- simulate_POMDP(
    solved, n = 4, horizon = 2, engine = "r", return_trajectories = TRUE
  )
  set.seed(2026)
  expect_message(
    sim_cpp <- simulate_POMDP(
      solved, n = 4, horizon = 2, engine = "cpp", return_trajectories = TRUE
    ),
    "Time-dependent"
  )
  expect_equal(sim_cpp$reward, sim_r$reward)
  expect_equal(sim_cpp$trajectories, sim_r$trajectories)
})

test_that("inconsistent episode definitions are rejected", {
  model <- make_time_dependent_tiger()
  model$transition_prob <- model$transition_prob[1]

  expect_error(
    pomdp:::check_and_fix_MDP(model),
    "appropriate number of episodes"
  )
})

make_dense_episode_model <- function() {
  reward_episode <- function(value) {
    list(wait = list(
      s1 = matrix(value, 2, 1),
      s2 = matrix(value, 2, 1)
    ))
  }

  POMDP(
    states = c("s1", "s2"),
    actions = "wait",
    observations = "seen",
    horizon = c(1, 1),
    transition_prob = list(
      list(wait = diag(2)),
      list(wait = matrix(c(0, 1, 1, 0), 2, byrow = TRUE))
    ),
    observation_prob = list(
      list(wait = matrix(1, 2, 1)),
      list(wait = matrix(1, 2, 1))
    ),
    reward = list(reward_episode(1), reward_episode(2))
  )
}

test_that("dense rewards can vary by episode", {
  model <- make_dense_episode_model()

  expect_equal(unname(reward_matrix(model, episode = 1)$wait$s1), matrix(1, 2, 1))
  expect_equal(unname(reward_matrix(model, episode = 2)$wait$s1), matrix(2, 2, 1))
})

test_that("malformed episode-specific fields are rejected", {
  valid <- make_dense_episode_model()

  missing_transition <- valid
  missing_transition$transition_prob[[2]] <- list(other = "identity")
  expect_error(
    pomdp:::check_and_fix_MDP(missing_transition),
    "transition_prob for action wait is missing"
  )

  bad_transition <- valid
  bad_transition$transition_prob[[2]]$wait <- matrix(.2, 2, 2)
  expect_error(
    pomdp:::check_and_fix_MDP(bad_transition),
    "rows do not add up"
  )

  missing_observation <- valid
  missing_observation$observation_prob[[2]] <- list(other = "uniform")
  expect_error(
    pomdp:::check_and_fix_MDP(missing_observation),
    "observation_prob for action wait is missing"
  )

  bad_observation <- valid
  bad_observation$observation_prob[[2]]$wait <- matrix(.5, 2, 1)
  expect_error(
    pomdp:::check_and_fix_MDP(bad_observation),
    "rows do not add up"
  )

  missing_reward <- valid
  missing_reward$reward[[2]]$wait$s2 <- NULL
  expect_error(
    pomdp:::check_and_fix_MDP(missing_reward),
    "state s2 in episode 2 is missing"
  )

  bad_reward <- valid
  bad_reward$reward[[2]]$wait$s1 <- matrix(0, 1, 1)
  expect_error(
    pomdp:::check_and_fix_MDP(bad_reward),
    "right dimensions"
  )
})
