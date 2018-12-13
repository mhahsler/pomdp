
### fix the broken curve_multiple for directed graphs (igraph_1.2.2)
  curve_multiple_fixed <- function(graph, start = 0.5) 
  {
    el <-  as_edgelist(graph, names = FALSE)
    o <- apply(el, 1, order)[1,]
    el <- apply(el, 1, FUN = function(x) paste(sort(x), collapse = ":"))
      cu <- ave(rep(NA, length(el)), el, FUN = function(x) {
        if (length(x) == 1) {
          return(0)
        }
        else {
          return(seq(-start, start, length = length(x)))
        }
      }
      )
      
      cu[o==2] <- cu[o==2] * -1
      cu
  }
  