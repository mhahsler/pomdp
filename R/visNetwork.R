# plot policy graph using visNetwork

# Note: legend is not used right now!
.plot.visNetwork <-
  function(x,
    belief = TRUE,
    legend = NULL,
    col = NULL,
    smooth = list(type = "continuous"),
    ...) {
    check_installed("visNetwork")
    
    pg <- policy_graph(x, belief = belief, col = col)
    
    ### add tooltip
    #V(pg)$title <- paste(htmltools::tags$b(V(pg)$label)
    V(pg)$title <- paste(V(pg)$label,
      lapply(
        V(pg)$pie,
        FUN = function(b) {
          knitr::kable(cbind(belief = b), digits = 3, format = "html")
        }
      ))
    
    ### colors
    if (belief) {
      # winner
      #V(pg)$color <- V(pg)$pie.color[[1]][sapply(V(pg)$pie, which.max)]
      
      # mixing in rgb spave
      V(pg)$color <- sapply(
        seq(length(V(pg))),
        FUN = function(i)
          grDevices::rgb(t(
            grDevices::col2rgb(V(pg)$pie.color[[1]]) %*% V(pg)$pie[[i]]
          ) / 255)
      )
      
      # mixing in hsv space
      #V(pg)$color <- sapply(seq(length(V(pg))), FUN = function(i)
      #  do.call(hsv, as.list(rgb2hsv(col2rgb(V(pg)$pie.color[[1]])) %*% V(pg)$pie[[i]])))
    }
    
    visNetwork::visIgraph(pg, idToLabel = FALSE, smooth = smooth, ...) %>%
      visNetwork::visOptions(
        highlightNearest = list(enabled = TRUE, degree = 0),
        nodesIdSelection = TRUE
      )
  }
