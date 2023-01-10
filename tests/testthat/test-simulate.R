library("testthat")
library("pomdp")


data("Tiger")

verb <- FALSE
#verb <- TRUE

### CRAN checker let's us use 2 processes
doParallel::registerDoParallel(2)

sim <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb)
sim <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb, method = 'r')

### sparse matrices
problem <- read_POMDP(system.file("examples/shuttle.95.POMDP", package = "pomdp"), parse = TRUE)
simulate_POMDP(problem, n = 100, horizon = 10, verbose = verb)
simulate_POMDP(problem, n = 100, horizon = 10, verbose = verb, method = 'r')

problem_norm <- normalize_POMDP(problem, sparse = FALSE)
system.time(simulate_POMDP(problem_norm, n = 100, horizon = 10, verbose = verb, method = 'r'))
