#' https://github.com/rafapereirabr/urban_centrality_index_2013/blob/master/uci_function_sf.R


library(sf)
library(fields)
library(accessibility)
library(data.table)
library(ggplot2)
library(furrr)
set.seed(42)

# read grid and land use data
grid <- system.file("extdata/grid_bho.rds", package = "accessibility")
grid <- readRDS(grid)

landuse <- system.file("extdata/land_use_data.rds", package = "accessibility")
landuse <- readRDS(landuse)

grid <- merge(grid, landuse, by = 'id')


b <- uci(sf_object = grid, var_name = 'jobs', full_border = TRUE)
#         UCI location_coef spatial_separation spatial_separation_max
# 1 0.2538635     0.5278007           3880.114               7475.899


a <- uci(sf_object = grid, var_name = 'jobs', full_border = F)
#         UCI location_coef spatial_separation spatial_separation_max
# 1 0.2557712     0.5278007           3880.114               7528.325

uci <- function(sf_object, var_name, full_border=FALSE){
  
# sf_object = grid
# var_name = 'jobs'
# full_border = FALSE

###### pre-calculations ###### -------------------------------------------------

# change projection to UTM
  sf_object <- suppressWarnings(sf::st_transform(sf_object, 3857))

### Determine which polygons are on the border
  # get perimeter of whole area
  boundary <- st_union(sf_object) |> sf::st_boundary()

# Determine border polygons
  int <- st_intersects(sf_object, boundary)
  border <- sapply(1:length(int),function(i){length(int[[i]])})
  sf_object$border <- border
  # plot(sf_object['border'], col=border)

# find total number and positions of cells in the border 
  total_cells <- sum(border)
  candidate_positions <- which(sf_object$border == 1, arr.ind=TRUE)


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
  

  
###### Support functions ######
  
  # Venables, Spatial Separation index
  venables <- function(b){
    v <- t(b) %*% distance %*% b
    return(v[1])
  }
  
  # Location Coefficient (LC)
  location_coef <- function(x){
    cl <-(sum(abs(x-(1/length(x)))))/2
    return(cl)
  }
  
  
  # sample positions
  sample_positions <- function(nbc, candidate_positions){ # nbc = 50
    
    postions <- sample(x = candidate_positions,
                       size = nbc, 
                       replace = FALSE)
    return(postions)
  }
  
  # simulate random spatial configurations / distributions
  simulate_spatial_config <- function(nbc, candidate_positions, output='vector', full_border=full_border){ # nbc = 100

    # sample spatial distribution of busy cells
    if( isTRUE(full_border) ) { positions <- candidate_positions; nbc <- length(candidate_positions) }
    if( isFALSE(full_border) ) { positions <- sample_positions(nbc = nbc, candidate_positions = candidate_positions) }
    
    if (output == 'spatial') {
      # number of jobs per cell
      jobs_per_cell <- 1 / nbc
      
      # allocate jobs
      temp_grid <- copy(sf_object)
      temp_grid$jobs_sim <- 0
      temp_grid[positions, ]$jobs_sim <- jobs_per_cell
      # plot(temp_grid['jobs_sim'])
      temp_grid$nbc <- nbc
      
      return(temp_grid)
    }
    
    if (output == 'vector') {
      # with all activities equally concentraded in 'positions'
      b <- border
      b <- rep(0, length(b))
      b[c(positions)] <- border[c(positions)]
      b[b == 1] <- 1 / length(b[b == 1])
      return(b)
      }
  
    }
  
  
### Find MAX spatial separation

  # IF we simplify max venables to consider full border
  if(isTRUE(full_border)) {
    
    b_full_border <- simulate_spatial_config(
      nbc = 1, # nbc does not matter
      candidate_positions,
      output = 'vector',
      full_border = full_border
    )
    max_venables <- venables_full_border <- venables(b_full_border)
  }
  
  # IF want to boostrap with N simulations
  if (isFALSE(full_border)) {
    
    set.seed(42)
    number_busy_cells <- 2:50
    all_sim_input <- rep(number_busy_cells, 200)
    
    # # linear
    all_sim <- pbapply::pblapply(
      X = all_sim_input,
      FUN = simulate_spatial_config,
      candidate_positions,
      output = 'vector',
      full_border = full_border
    )
    
    # # parallel
    # future::plan(future::multisession)
    # options(future.globals.maxSize= 891289600) # 850 MB
    # all_sim <- furrr::future_map(.x = all_sim_input,
    #                               .f = simulate_spatial_config,
    #                               candidate_positions,
    #                               output = 'vector',
    #                               full_border = full_border,
    #                               .progress = TRUE,
    #                               .options = furrr_options(seed = 42))
                       

    # calculate venables of all simulations and get the max value
    
    # linear
    all_sim_venables <- pbapply::pblapply(X = all_sim, FUN = venables)
    
    # # parallel
    # all_sim_venables <- furrr::future_map(.x = all_sim, .f = venables, .progress = TRUE)
    
    all_sim_venables <- unlist(all_sim_venables)
    max_venables <- max(all_sim_venables)
    
  }
  



# 
# ## explore
# all_sim_LC <- pbapply::pblapply(X = all_sim, FUN = location_coef)
# all_sim_LC <- unlist(all_sim_LC)
# 
# temp_df <- data.table('nbc' = all_sim_input,
#                       'venables' = all_sim_venables,
#                       'lc' = all_sim_LC)
# 
# temp_df[, v_max := max(venables), by = nbc]
# temp_df[, proximity_idex  := 1-(venables/v_max)]
# temp_df[, UCI := lc * proximity_idex]
# 
# # #6666 DOES IT MAKE a difference using boostrap or border? only in 3rd decimal case
# # temp_df[, proximity_idex  := 1-(venables/max_venables)]
# # temp_df[, UCI_global := lc * proximity_idex]
# # 
# # temp_df[, proximity_idex  := 1-(venables/venables_full_border)]
# # temp_df[, UCI_border := lc * proximity_idex]
# # 
# # 
# # temp_df[,  UCI_global - UCI_border ] |> summary()
# # #6666
# 
# summary(temp_df$proximity_idex)
# plot(density(temp_df$proximity_idex), col='red')
# 
# subset(temp_df, venables == max_venables)
# subset(temp_df, UCI == max(temp_df$UCI))
# subset(temp_df, UCI == min(temp_df$UCI))
# 
#   ggplot(data=temp_df) +
#     geom_point(aes(x=nbc, y=lc), alpha=.3)
# 
#   ggplot(data=temp_df) +
#     geom_point(aes(x=nbc, y=venables), alpha=.3)
# 
#   ggplot(data=temp_df) +
#     geom_point(aes(x=nbc, y=proximity_idex), alpha=.3)
# 
#   ggplot(data=temp_df) +
#     geom_point(aes(x=venables, y=lc), alpha=.3)
# 
#   ggplot(data=temp_df) +
#     geom_point(aes(x=lc, y=proximity_idex), alpha=.3)
# 
#   ggplot(data=temp_df) +
#     geom_point(aes(x=nbc, y=v_max), alpha=.3)
# 
#   ggplot(data=temp_df) +
#     geom_point(aes(x=lc, y=v_max), alpha=.3)
# 
#   plotly::plot_ly(
#                   x = temp_df$UCI,
#                   y = temp_df$proximity_idex,
#                   z = temp_df$nbc,
#                   type = "scatter3d",
#                   mode = "markers",
#                   color = temp_df$UCI
#                 )
#   

###### UCI and its components ######

## location coefficient
LC <- location_coef(var_x_norm)

# Spatial separation index (venables)
v <- venables(var_x_norm)

# Proximity Index PI
proximity_idex <- 1-(v/max_venables)

# UCI
UCI <- LC * proximity_idex

output_df <- data.frame(
  UCI = UCI,
  location_coef = LC,
  spatial_separation = v,
  spatial_separation_max = max_venables
)

return(output_df)
}

### TO DO

#' 1) how do can we retrieve from the simulations the 
#' spatial distribution that maximizes UCI or vmax ?

#' 2) na hora de achar v_max, checar se resultado é o mesmo se calcular venables
#'  com matriz de distancia de todas celulas ou se usarmos so celulas da borda


