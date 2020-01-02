# plot policy graph using visNetwork

# Note: legend is not used right now!
.plot.visNetwork <- function(x,  belief = TRUE, legend = NULL, cols = NULL, ...) {
  
  if(is.finite(x$solution$horizon)) stop("Only infinite horizon POMDPs have a plotable policy graph!")
  
  pg <- policy_graph(x, belief = belief, cols = cols)

  ### add tooltip
  #V(pg)$title <- paste(htmltools::tags$b(V(pg)$label)
  V(pg)$title <- paste(V(pg)$label,
    lapply(V(pg)$pie, FUN = function(b) {
      knitr::kable(cbind(belief = b), digits =3, format = "html")
    })
  )
 
  ### colors 
  # winner
  #V(pg)$color <- V(pg)$pie.color[[1]][sapply(V(pg)$pie, which.max)]
  
  # mixing in rgb spave
  V(pg)$color <- sapply(seq(length(V(pg))), FUN = function(i) 
    grDevices::rgb(t(grDevices::col2rgb(V(pg)$pie.color[[1]]) %*% V(pg)$pie[[i]])/255))
 
  # mixing in hsv space 
  #V(pg)$color <- sapply(seq(length(V(pg))), FUN = function(i) 
  #  do.call(hsv, as.list(rgb2hsv(col2rgb(V(pg)$pie.color[[1]])) %*% V(pg)$pie[[i]])))
  
  visNetwork::visIgraph(pg, idToLabel = FALSE, ...) %>% 
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 0),
      nodesIdSelection = TRUE)
}
