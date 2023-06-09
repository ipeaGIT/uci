#' @keywords internal
assert_var_name <- function(sf_object, var_name) {

  checkmate::assert_data_frame(sf_object)
  checkmate::assert_class(sf_object, 'sf')
  checkmate::assert_names(
    names(sf_object),
    must.include = var_name,
    .var.name = "sf_object"
  )

  return(invisible(TRUE))
}


# Location Coefficient (LC)
#' @keywords internal
location_coef <- function(x){
  cl <-(sum(abs(x-(1/length(x)))))/2
  return(cl)
}

# Venables, Spatial Separation index
#' @keywords internal
venables <- function(b, distance){
  v <- t(b) %*% distance %*% b
  return(v[1])
}


# calculate distance matrix
#' @keywords internal
get_distance_matrix <- function(sf_object){
  
  coords <- suppressWarnings(sf::st_coordinates( sf::st_centroid(sf_object) ))
  distance <- fields::rdist(coords)
  # plot(coords)
  
  # self distance >> REF Crafts & Mulatu (2006)
  # IMPORTANT: the area must be in the same unit as the distance matrix####
  n_reg <- nrow(distance)
  poly_areas <- sf::st_area(sf_object)
  self <- diag((poly_areas/pi)^(1/2), nrow=n_reg, ncol=n_reg)
  distance <- distance+self ## Sum distance matrix and self-distance
  return(distance)
}


# normalize distribution of variable
#' @keywords internal
normalize_distribution <- function(vec) {
  var_x <- matrix(vec, length(vec), 1)
  var_x_norm <- var_x / sum(var_x) # normalization
  return(var_x_norm)
}


# sample positions
#' @keywords internal
sample_positions <- function(nbc, candidate_positions){ # nbc = 50
  
  postions <- sample(x = candidate_positions,
                     size = nbc, 
                     replace = FALSE)
  return(postions)
}




# simulate random spatial configurations / distributions along border
#' @keywords internal
simulate_border_config <- function(sf_object, nbc, output='vector', full_border=full_border){ # nbc = 100
  
  # find positions of cells in the border 
  candidate_positions <- which(sf_object$border == 1, arr.ind=TRUE)
  
  # sample spatial distribution of busy cells
  if( isTRUE(full_border) ) { positions <- candidate_positions; nbc <- length(candidate_positions) }
  if( isFALSE(full_border) ) { positions <- sample_positions(nbc = nbc, candidate_positions = candidate_positions) }
  
  if (output == 'spatial') {
    # number of jobs per cell
    jobs_per_cell <- 1 / nbc
    
    # allocate jobs
    sf_object$jobs_sim <- 0
    sf_object[positions, ]$jobs_sim <- jobs_per_cell
    # plot(sf_object['jobs_sim'])
    sf_object$nbc <- nbc
    
    return(sf_object)
  }
  
  if (output == 'vector') {
    # with all activities equally concentraded in 'positions'
    b <- rep(0, nrow(sf_object))
    b[c(positions)] <- 1
    b[b == 1] <- 1 / length(b[b == 1])
    return(b)
  }
  
}
