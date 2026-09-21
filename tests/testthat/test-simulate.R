test_that("R and C++ POMDP simulators give comparable results", {
set.seed(42)

data("Tiger")

verb <- FALSE
#verb <- TRUE

### CRAN checker let's us use 2 processes
### but windows cannot do it!
# doParallel::registerDoParallel(2)

sim_c <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb)
sim_r <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb, engine = 'r')

#mean(sim_c$reward)
#mean(sim_r$reward)
expect_lt(abs(mean(sim_c$reward) - mean(sim_r$reward)) / mean(sim_c$reward), 0.2)

### sparse matrices
problem <- read_POMDP(system.file("examples/shuttle_95.POMDP", package = "pomdp"), parse = TRUE)
sim_c <- simulate_POMDP(problem, n = 100, horizon = 10, verbose = verb)
sim_r <- simulate_POMDP(problem, n = 100, horizon = 10, verbose = verb, engine = 'r')

#mean(sim_c$reward)
#mean(sim_r$reward)
expect_lt(abs(mean(sim_c$reward) - mean(sim_r$reward)) / mean(sim_c$reward), 0.3)

problem_norm <- normalize_POMDP(problem, sparse = FALSE)
expect_type(
  simulate_POMDP(problem_norm, n = 10, horizon = 3, verbose = verb, engine = 'r'),
  "list"
)

# deregister backend
### foreach::registerDoSEQ()
})

test_that("R and C++ simulators agree exactly for deterministic models", {
  mdp <- MDP(
    states = c("left", "right"),
    actions = "wait",
    transition_prob = list(wait = matrix(c(0, 1, 1, 0), 2, byrow = TRUE)),
    reward = R_("wait", value = 2),
    discount = .9,
    horizon = 4,
    start = "left"
  )

  set.seed(17)
  mdp_r <- simulate_MDP(
    mdp, n = 5, engine = "r", return_trajectories = TRUE
  )
  set.seed(17)
  mdp_cpp <- simulate_MDP(
    mdp, n = 5, engine = "cpp", return_trajectories = TRUE
  )
  expect_equal(mdp_cpp$reward, mdp_r$reward)
  expect_equal(mdp_cpp$action_cnt, mdp_r$action_cnt)
  expect_equal(mdp_cpp$state_cnt, mdp_r$state_cnt)
  expect_output(
    simulate_MDP(mdp, n = 1, engine = "r", verbose = TRUE),
    "Simulating MDP trajectories"
  )

  pomdp <- POMDP(
    states = c("left", "right"),
    actions = "wait",
    observations = "seen",
    transition_prob = list(wait = matrix(c(0, 1, 1, 0), 2, byrow = TRUE)),
    observation_prob = list(wait = "uniform"),
    reward = R_("wait", value = 2),
    discount = .9,
    horizon = 4,
    start = "left"
  )

  set.seed(23)
  pomdp_r <- simulate_POMDP(
    pomdp, n = 5, engine = "r", return_beliefs = TRUE,
    return_trajectories = TRUE
  )
  set.seed(23)
  pomdp_cpp <- simulate_POMDP(
    pomdp, n = 5, engine = "cpp", return_beliefs = TRUE,
    return_trajectories = TRUE
  )
  expect_equal(pomdp_cpp$reward, pomdp_r$reward)
  expect_equal(pomdp_cpp$state_cnt, pomdp_r$state_cnt)
  expect_equal(pomdp_cpp$belief_states, pomdp_r$belief_states)
  expect_output(
    simulate_POMDP(pomdp, n = 1, engine = "r", verbose = TRUE),
    "Simulating POMDP trajectories"
  )
})

test_that("simulation validates horizon, policy, and belief arguments", {
  data("Tiger")
  data("Maze")

  no_discounting <- Tiger
  no_discounting$discount <- 1
  expect_error(
    simulate_POMDP(no_discounting, horizon = Inf),
    "finite simulation horizon"
  )
  expect_error(
    simulate_POMDP(Tiger, horizon = 2, epsilon = .5),
    "epsilon has to be 1"
  )
  expect_error(
    simulate_POMDP(Tiger, belief = c(.2, .2), horizon = 2),
    "add up to 1"
  )
  expect_error(
    simulate_MDP(Maze, horizon = 2, epsilon = .5),
    "epsilon has to be 1"
  )
  expect_error(simulate_MDP(Maze, engine = "gpu"), "arg")
})

test_that("functional models warn and fall back to the R simulator", {
  transition <- function(action, start.state, end.state) {
    as.numeric(start.state != end.state)
  }
  reward <- function(action, start.state, end.state) 1
  mdp <- MDP(c("s1", "s2"), "wait", transition, reward,
    horizon = 2, start = "s1")

  expect_warning(
    sim <- simulate_MDP(mdp, n = 2, engine = "cpp"),
    "Falling back to R"
  )
  expect_equal(sim$reward, rep(1.9, 2))
})
