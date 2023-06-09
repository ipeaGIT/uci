
a <- simulate_spatial_config(sf_object,
                             nbc = 10,
                             output='spatial', 
                             border_only=T
                             )

plot(a['jobs_sim'])

# simulate random spatial configurations / distributions
simulate_spatial_config <- function(sf_object, nbc, output='vector', border_only=FALSE){ # nbc = 100
  
  # we need to determine border cells if output is a vector too
  if (isTRUE(output=='vector' | border_only)) {
    
    # determine border cells
    boundary <- st_union(sf_object) |> sf::st_boundary()
    int <- st_intersects(sf_object, boundary)
    border <- sapply(1:length(int),function(i){length(int[[i]])})
    sf_object$border <- border
    # plot(sf_object['border'], col=border)
  }
    
  ## sample spatial distribution of busy cells
  # any cell
    if (isFALSE(border_only)) {
      candidate_positions <- nrow(sf_object)
      positions <- sample_positions(nbc = nbc, candidate_positions = candidate_positions)
      }
  
  # only cells along border
  if (isTRUE(border_only)) { 
    candidate_positions <- which(sf_object$border == 1, arr.ind=TRUE) # positions of cells in the border
    positions <- sample_positions(nbc = nbc, candidate_positions = candidate_positions)
  }
  
  # spatial sf output
  if (output == 'spatial') {
    # number of jobs per cell
    jobs_per_cell <- 1 / nbc
    
    # allocate jobs
    sf_object$jobs_sim <- 0
    sf_object[positions, ]$jobs_sim <- jobs_per_cell
    # plot(sf_object['jobs_sim'])
    return(sf_object)
  }
  
  # vector output
  if (output == 'vector') {
    # with all activities equally concentraded in 'positions'
    b <- rep(0, length(border))
    b[c(positions)] <- 1
    b[b == 1] <- 1 / length(b[b == 1])
    return(b)
  }
  
}
