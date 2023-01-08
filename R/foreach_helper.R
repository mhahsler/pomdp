
# splits a job of n replication into equally sized jobs for all foreach workers
# if more than min jobs are requested!
# returns a vector with the number of replications for each worker
foreach_split <- function(n, min = 100) {
  if (n <= min)
    return(n)
    
  nw <- foreach::getDoParWorkers()
  ns <- rep(ceiling(n / nw), times = nw)
  dif <- sum(ns) - n
  if (dif > 0)
    ns[1:dif] <- ns[1:dif] - 1
  
  ns
}