library("testthat")
library("pomdp")

data("Tiger")

verb <- FALSE
#verb <- TRUE

### CRAN checker let's us use 2 processes
doParallel::registerDoParallel(2)

sim_c <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb)
sim_r <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb, engine = 'r')

#mean(sim_c$reward)
#mean(sim_r$reward)
expect_lt(abs(mean(sim_c$reward) - mean(sim_r$reward)) / mean(sim_c$reward), 0.2)

### sparse matrices
problem <- read_POMDP(system.file("examples/shuttle.95.POMDP", package = "pomdp"), parse = TRUE)
sim_c <- simulate_POMDP(problem, n = 100, horizon = 10, verbose = verb)
sim_r <- simulate_POMDP(problem, n = 100, horizon = 10, verbose = verb, engine = 'r')

#mean(sim_c$reward)
#mean(sim_r$reward)
expect_lt(abs(mean(sim_c$reward) - mean(sim_r$reward)) / mean(sim_c$reward), 0.3)

problem_norm <- normalize_POMDP(problem, sparse = FALSE)
system.time(simulate_POMDP(problem_norm, n = 100, horizon = 10, verbose = verb, engine = 'r'))

doParallel::stopImplicitCluster()
