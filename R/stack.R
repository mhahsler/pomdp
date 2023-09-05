# R Stack implementation following the queue from: 
#   https://www.researchgate.net/post/What_is_the_queue_data_structure_in_R

new.stack <- function() {
  ret <- new.env()
  ret$top <- NULL
  ret$n <- 0L
  return(ret)
}

delete.stack <- function(stack) {
  rm(stack)
}

push.stack <- function(stack, add){
  element <- new.env()
  element$val <- add
  element$nxt <- stack$top
  stack$top <- element
  stack$n <- stack$n + 1L
}

pop.stack <- function(stack){
  if (empty.stack(stack))
    stop("Attempting to take element from an empty stack")
  
  element <- stack$top
  value <- element$val
  
  stack$top <- element$nxt
  stack$n <- stack$n - 1L
  return(value)
}

empty.stack <- function(stack)
  return(stack$n < 1L)

# Example code:
# N <- 10
# system.time({
#   x <- new.stack()
#   for(i in 1:N){
#     push.stack(x,c(i, i))
#   }
#   while(!empty.stack(x)){
#     cat(pop.stack(x), "\n")
#   }
# })
# 
# delete.stack(x)

# delete.queue(x)