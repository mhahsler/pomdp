library("testthat")
library("pomdp")

m <- matrix(1:12, nrow = 4)
v1 <- 1:3
v2 <- 1:4

#pomdp:::vecprod(m, v1)
expect_identical(pomdp:::vecprod(m, v1), drop(m %*% v1))
expect_error(pomdp:::vecprod(m, v2))

#pomdp:::veccrossprod(m, v2)
expect_identical(pomdp:::veccrossprod(m, v2), drop(crossprod(m, v2)))
expect_error(pomdp:::veccrossprod(m, v1))

## context("belief")

data(Tiger)
Tiger_norm <- normalize_POMDP(Tiger)

# listen, observe tiger to the left
#pomdp:::update_belief_cpp(Tiger_norm, c(.5, .5), 0, 0)
#unname(update_belief(Tiger, c(.5, .5), 1, 1))

expect_identical(pomdp:::update_belief_cpp(Tiger_norm, c(.5, .5), 0, 0), 
  unname(update_belief(Tiger, c(.5, .5), 1, 1)))

#pomdp:::update_belief_cpp(Tiger_norm, c(.85, .15), 0, 0)
#unname(update_belief(Tiger, c(.85, .15), 1, 1))

expect_identical(pomdp:::update_belief_cpp(Tiger_norm, c(.85, .15), 0, 0), 
  unname(update_belief(Tiger, c(.85, .15), 1, 1)))

## simulate_POMDP
data(Tiger)

verb <- FALSE

# unsolved
simulate_POMDP(Tiger, n = 10, horizon = 10, verbose = verb, engine = "r")
simulate_POMDP(Tiger, n = 10, horizon = 10, verbose = verb, engine = "cpp")

#simulate_POMDP(Tiger, n = 1000, horizon = 10, engine = "r")$avg
#simulate_POMDP(Tiger, n = 1000, horizon = 10, engine = "cpp")$avg


# converged solution
sol <- solve_POMDP(Tiger)
sol
policy(sol)

simulate_POMDP(sol, n = 10, horizon = 10, verbose = verb, engine = "r")
simulate_POMDP(sol, n = 10, horizon = 10, verbose = verb, engine = "cpp")

#simulate_POMDP(sol, n = 1000, horizon = 10, engine = "r")$avg
#simulate_POMDP(sol, n = 1000, horizon = 10, engine = "cpp")$avg

## fixed horizon solution
sol <- solve_POMDP(Tiger, horizon = 5, discount = 1, method = "enum")
sol
policy(sol)

simulate_POMDP(sol, n = 10, verbose = verb, engine = "r")
simulate_POMDP(sol, n = 10, verbose = verb, engine = "cpp")

#simulate_POMDP(sol, n = 1000, engine = "r")$avg
#simulate_POMDP(sol, n = 1000, engine = "cpp")$avg

## simulate_MDP

data(Maze)
verb <- FALSE

# unsolved MDP
simulate_MDP(Maze, n = 10, horizon = 10, verbose = verb, engine = "r")
simulate_MDP(Maze, n = 10, horizon = 10, return_states = TRUE, verbose = verb, engine = "r")

simulate_MDP(Maze, n = 10, horizon = 10, verbose = verb, engine = "cpp")
simulate_MDP(Maze, n = 10, horizon = 10, return_states = TRUE, verbose = verb, engine = "cpp")

#microbenchmark::microbenchmark(simulate_MDP(Maze, n = 100, horizon = 10, verbose = FALSE, engine = "r"))
#microbenchmark::microbenchmark(simulate_MDP(Maze, n = 100, horizon = 10, verbose = FALSE, engine = "cpp"))

# solved MDP
sol <- solve_MDP(Maze, discount = 1)

simulate_MDP(sol, n = 10, horizon = 10, verbose = verb)
