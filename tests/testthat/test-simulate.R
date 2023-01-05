library("testthat")
library("pomdp")


data("Tiger")

verb <- FALSE
#verb <- TRUE

### CRAN checker let's us use 2 processes
doParallel::registerDoParallel(2)

system.time(sim <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb))
system.time(sim <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb, method = 'r'))

## rsparse is very slow!
system.time(sim <- simulate_POMDP(Tiger, n = 1000, horizon = 5, return_beliefs = TRUE, verbose = verb, method = 'r-sparse'))


