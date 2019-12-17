# Visualization using visNetwork

.installed <- function(pkg) !inherits(try(utils::installed.packages()[pkg,],
  silent=TRUE), "try-error")

visNetwork_POMDP <- function(x,  belief = TRUE, cols = NULL, ...) {
  if(!.installed("visNetwork")) stop("Package 'visNetwork' needs to be  installed!")
  
  if(is.finite(x$solution$horizon)) stop("Only infinite horizon POMDPs have a plotable policy graph!")
  
  pg <- policy_graph(x, belief = belief, cols = cols)

  ### add tooltip
  V(pg)$title <- sapply(V(pg)$pie, FUN = function(x) paste("<b>Belief Proportions</b><p>", 
    paste(names(x), "=", round(x,2), collapse = "<br>")))
  
  V(pg)$color <- V(pg)$pie.color[[1]][sapply(V(pg)$pie, which.max)]

  visNetwork::visIgraph(pg, idToLabel = FALSE, ...) %>% 
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 0, hover = TRUE),
      nodesIdSelection = TRUE)
}
