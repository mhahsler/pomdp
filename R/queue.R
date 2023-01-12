# R Queue implementation following: https://www.researchgate.net/post/What_is_the_queue_data_structure_in_R

new.queue <- function() {
  ret <- new.env()
  ret$front <- new.env()
  ret$front$q <- NULL
  ret$front$prev <- NULL
  ret$last <- ret$front
  return(ret)
}

delete.queue <- function(queue) {
  rm(queue)
}

enqueue <- function(queue, add){
  queue$last$q <- new.env()
  queue$last$q$prev <- queue$last
  queue$last <- queue$last$q
  queue$last$val <- add
  queue$last$q <- NULL
}

dequeue <- function(queue){
  if (queue.empty(queue)) {
    stop("Attempting to take element from empty queue")
  }
  value <- queue$front$q$val
  queue$front <- queue$front$q
  queue$front$q$prev <- NULL
  return(value)
}

queue.empty <- function(queue){
  return(is.null(queue$front$q))
}

# N <- 10
# system.time({
#   x <- new.queue()
#   for(i in 1:N){
#     enqueue(x,c(i, i))
#   }
#   for(i in 1:N){
#     cat(dequeue(x), "\n")
#   }
# })
# delete.queue(x)