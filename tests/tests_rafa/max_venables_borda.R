# Venables, Spatial Separation index
venables <- function(b){
  v <- t(b) %*% distance %*% b
  return(v[1])
}

# soh nao vazias e cheio dá igual !!!!


# cheio -------------------------------------------------------------------------
sf_object <- grid
plot(sf_object['jobs'])

# change projection to UTM
sf_object <- suppressWarnings(sf::st_transform(sf_object, 3857))

### normalize distribution of variable
var_x <- sf_object[var_name][[1]]
var_x <- matrix(var_x, length(var_x),1)
var_x_norm <- var_x/sum(var_x) # normalization


### calculate distance matrix
coords <- suppressWarnings(st_coordinates( st_centroid(sf_object) ))
distance <- fields::rdist(coords)
# plot(coords)

# self distance >> REF Crafts & Mulatu (2006)
# IMPORTANT: the area must be in the same unit as the distance matrix####
n_reg <- nrow(distance)
poly_areas <- st_area(sf_object)
self <- diag((poly_areas/pi)^(1/2), nrow=n_reg, ncol=n_reg)
distance <- distance+self ## Sum distance matrix and self-distance

v_cheio <- venables(var_x_norm)


# soh nao vazias  -------------------------------------------------------------------------
    sf_object <- grid
    sf_object <- subset(sf_object, jobs >0)
    plot(sf_object['jobs'])
    
    # change projection to UTM
    sf_object <- suppressWarnings(sf::st_transform(sf_object, 3857))
    
    
    
    ### normalize distribution of variable
    var_x <- sf_object[var_name][[1]]
    var_x <- matrix(var_x, length(var_x),1)
    var_x_norm <- var_x/sum(var_x) # normalization
    
    
    ### calculate distance matrix
    coords <- suppressWarnings(st_coordinates( st_centroid(sf_object) ))
    distance <- fields::rdist(coords)
    # plot(coords)
    
    # self distance >> REF Crafts & Mulatu (2006)
    # IMPORTANT: the area must be in the same unit as the distance matrix####
    n_reg <- nrow(distance)
    poly_areas <- st_area(sf_object)
    self <- diag((poly_areas/pi)^(1/2), nrow=n_reg, ncol=n_reg)
    distance <- distance+self ## Sum distance matrix and self-distance

    v_nvazias <- venables(var_x_norm)
    
    
    
    
    

    
    
    
    
# BORDA all  -------------------------------------------------------------------------
    sf_object <- grid
    plot(sf_object['jobs'])
    
    # change projection to UTM
    sf_object <- suppressWarnings(sf::st_transform(sf_object, 3857))
    
    # Determine border polygons
    boundary <- st_union(sf_object) |> sf::st_boundary()
    int <- st_intersects(sf_object, boundary)
    border <- sapply(1:length(int),function(i){length(int[[i]])})
    sf_object$border <- border
    plot(sf_object['border'], col=border)
    
    # find total number and positions of cells in the border 
    total_cells <- sum(border)
    candidate_positions <- which(sf_object$border == 1, arr.ind=TRUE)
    
    sf_object <- simulate_spatial_config(
      nbc = 1, # nbc does not matter
      candidate_positions,
      output = 'spatial',
      full_border = TRUE
    )
    plot(sf_object['border'], col=border)
    
    ### normalize distribution of variable
    var_x <- sf_object[var_name][[1]]
    var_x <- matrix(var_x, length(var_x),1)
    var_x_norm <- var_x/sum(var_x) # normalization
    
    
    ### calculate distance matrix
    coords <- suppressWarnings(st_coordinates( st_centroid(sf_object) ))
    distance <- fields::rdist(coords)
    # plot(coords)
    
    # self distance >> REF Crafts & Mulatu (2006)
    # IMPORTANT: the area must be in the same unit as the distance matrix####
    n_reg <- nrow(distance)
    poly_areas <- st_area(sf_object)
    self <- diag((poly_areas/pi)^(1/2), nrow=n_reg, ncol=n_reg)
    distance <- distance+self ## Sum distance matrix and self-distance
    

    v_border_all <- venables(var_x_norm)
    v_border_all
    7475.899
    
    
    
    
    
    
    
  
    
    
    
# BORDA only  -------------------------------------------------------------------------
    sf_object <- grid
    plot(sf_object['jobs'])
    
    # change projection to UTM
    sf_object <- suppressWarnings(sf::st_transform(sf_object, 3857))
    
    # Determine border polygons
    boundary <- st_union(sf_object) |> sf::st_boundary()
    int <- st_intersects(sf_object, boundary)
    border <- sapply(1:length(int),function(i){length(int[[i]])})
    sf_object$border <- border
    plot(sf_object['border'])
    
    # find total number and positions of cells in the border 
    total_cells <- sum(border)
    candidate_positions <- which(sf_object$border == 1, arr.ind=TRUE)
    
    sf_object <- simulate_spatial_config(
      nbc = 1, # nbc does not matter
      candidate_positions,
      output = 'spatial',
      full_border = TRUE
    )
    plot(sf_object['border'])
    
    sf_object <- subset(sf_object, border==1)
    plot(sf_object['border'])
    
    ### normalize distribution of variable
    var_x <- sf_object[var_name][[1]]
    var_x <- matrix(var_x, length(var_x),1)
    var_x_norm <- var_x/sum(var_x) # normalization
    
    
    ### calculate distance matrix
    coords <- suppressWarnings(st_coordinates( st_centroid(sf_object) ))
    distance <- fields::rdist(coords)
    # plot(coords)
    
    # self distance >> REF Crafts & Mulatu (2006)
    # IMPORTANT: the area must be in the same unit as the distance matrix####
    n_reg <- nrow(distance)
    poly_areas <- st_area(sf_object)
    self <- diag((poly_areas/pi)^(1/2), nrow=n_reg, ncol=n_reg)
    distance <- distance+self ## Sum distance matrix and self-distance
    
    
    v_border_only <- venables(var_x_norm)
    v_border_only
    7475.899