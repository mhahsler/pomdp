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

test_that("unreachable states are removed from all MDP representations", {
  keyword_model <- MDP(
    states = c("a", "b", "c"),
    actions = c("move", "stay"),
    transition_prob = list(
      move = rbind(
        c(0, 1, 0),
        c(0, 1, 0),
        c(0, 0, 1)
      ),
      stay = "identity"
    ),
    reward = rbind(
      R_(value = -1),
      R_(start.state = "b", end.state = "b", value = 5),
      R_(start.state = "c", value = 9)
    ),
    start = "b"
  )

  data_frame_model <- MDP(
    states = keyword_model$states,
    actions = keyword_model$actions,
    transition_prob = rbind(
      T_("move", "a", "b", 1),
      T_("move", "b", "b", 1),
      T_("move", "c", "c", 1),
      T_("stay", "a", "a", 1),
      T_("stay", "b", "b", 1),
      T_("stay", "c", "c", 1)
    ),
    reward = keyword_model$reward,
    start = keyword_model$start
  )

  function_model <- keyword_model
  function_model$transition_prob <- function(action, start.state, end.state) {
    transition_val(keyword_model, action, start.state, end.state)
  }
  function_model$reward <- function(action, start.state, end.state) {
    reward_val(keyword_model, action, start.state, end.state)
  }

  models <- list(
    data_frame = data_frame_model,
    dense = normalize_MDP(keyword_model, sparse = FALSE, trans_keyword = TRUE),
    sparse = normalize_MDP(keyword_model, sparse = TRUE, trans_keyword = TRUE),
    keyword = keyword_model,
    function_model = function_model
  )
  reduced <- lapply(models, remove_unreachable_states)

  expect_equal(
    reachable_states(keyword_model),
    c(a = FALSE, b = TRUE, c = FALSE)
  )
  expect_true(all(vapply(
    reduced,
    function(model) identical(model$states, "b"),
    logical(1)
  )))

  canonical <- function(model) {
    list(
      transition = transition_matrix(model, sparse = FALSE),
      reward = reward_matrix(model, sparse = FALSE),
      start = start_vector(model)
    )
  }
  expected <- canonical(reduced$data_frame)
  for (model in reduced)
    expect_equal(canonical(model), expected)

  expect_s3_class(reduced$data_frame$transition_prob, "data.frame")
  expect_true(is.list(reduced$dense$reward))
  expect_s4_class(reduced$sparse$transition_prob$move, "dgCMatrix")
  expect_identical(reduced$keyword$transition_prob$stay, "identity")
  expect_true(is.function(reduced$function_model$transition_prob))
  expect_true(is.function(reduced$function_model$reward))
})

test_that("unreachable POMDP states are removed without changing observations", {
  model <- MDP(
    states = c("a", "b", "c"),
    actions = "move",
    transition_prob = list(move = rbind(
      c(0, 1, 0),
      c(0, 1, 0),
      c(0, 0, 1)
    )),
    reward = rbind(
      R_(value = -1),
      R_(start.state = "b", end.state = "b", value = 5)
    ),
    start = "b"
  )
  pomdp <- make_partially_observable(model)
  dense <- normalize_POMDP(pomdp, sparse = FALSE, trans_keyword = TRUE)
  sparse <- normalize_POMDP(pomdp, sparse = TRUE, trans_keyword = TRUE)
  dense$terminal_values <- c(a = 1, b = 2, c = 3)

  function_model <- pomdp
  function_model$transition_prob <- function(action, start.state, end.state) {
    transition_val(pomdp, action, start.state, end.state)
  }
  function_model$observation_prob <- function(action, end.state, observation) {
    observation_val(pomdp, action, end.state, observation)
  }
  function_model$reward <- function(action, start.state, end.state, observation) {
    reward_val(pomdp, action, start.state, end.state, observation)
  }

  data_frame_model <- POMDP(
    states = pomdp$states,
    actions = pomdp$actions,
    observations = pomdp$observations,
    transition_prob = pomdp$transition_prob,
    observation_prob = rbind(
      O_("move", "a", "a", 1),
      O_("move", "b", "b", 1),
      O_("move", "c", "c", 1)
    ),
    reward = pomdp$reward,
    start = pomdp$start
  )

  keyword_model <- pomdp
  keyword_model$observation_prob <- list(move = "uniform")

  reduced_dense <- remove_unreachable_states(dense)
  reduced_sparse <- remove_unreachable_states(sparse)
  reduced_function <- remove_unreachable_states(function_model)
  reduced_data_frame <- remove_unreachable_states(data_frame_model)
  reduced_keyword <- remove_unreachable_states(keyword_model)

  expect_identical(reduced_dense$states, "b")
  expect_identical(reduced_dense$observations, c("a", "b", "c"))
  expect_equal(dim(reduced_dense$observation_prob$move), c(1L, 3L))
  expect_equal(dim(reduced_dense$reward$move$b), c(1L, 3L))
  expect_identical(reduced_dense$terminal_values, c(b = 2))
  expect_equal(
    observation_matrix(reduced_sparse, sparse = FALSE),
    observation_matrix(reduced_dense, sparse = FALSE)
  )
  expect_equal(
    reward_matrix(reduced_sparse, sparse = FALSE),
    reward_matrix(reduced_dense, sparse = FALSE)
  )
  expect_equal(
    observation_matrix(reduced_function, sparse = FALSE),
    observation_matrix(reduced_dense, sparse = FALSE)
  )
  expect_equal(
    reward_matrix(reduced_function, sparse = FALSE),
    reward_matrix(reduced_dense, sparse = FALSE)
  )
  expect_s3_class(reduced_data_frame$observation_prob, "data.frame")
  expect_identical(reduced_keyword$observation_prob$move, "uniform")
  expect_equal(
    dim(observation_matrix(reduced_keyword, sparse = FALSE)$move),
    c(1L, 3L)
  )
})
