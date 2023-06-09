#' Urban Centrality Index
#'
#' Calculates the Urban Centrality Index (UCI) as in Pereira et al., (2013) <https://doi.org/10.1111/gean.12002>. The UCI measures the 
#' extent to which the spatial organization of a city or region varies from 
#' extreme monocentric to extreme polycentric in a continuous scale from 0 to 1. 
#' Values close to 0 indicate more polycentric patterns and values close to 1 
#' indicate a more monocentric urban form.
#' 
#' @param sf_object A `POLYGON sf data.frame` of the study area.
#' @param var_name A `string`. The name of the column in `sf_object` with the 
#'        number of activities/opportunities/resources/services to be considered 
#'        when calculating urban centrality levels.
#' @param full_border A `lo`. t
#'
#' @family urban centrality index
#'
#' @examples
#' # load data
#' data_dir <- system.file("extdata", package = "uci")
#' grid <- readRDS(file.path(data_dir, "grid_bho.rds"))
#'
#' # calculate UCI
#'  df <- uci(
#'         sf_object = grid,
#'         var_name = 'jobs',
#'         full_border = TRUE
#'         )
#' head(df)
#'
#' @export
uci <- function(sf_object, 
                var_name, 
                full_border = FALSE
                ){
  
  # check inputs
  checkmate::assert_class(sf_object, 'sf')
  checkmate::assert_string(var_name)
  checkmate::assert_logical(full_border, len = 1, any.missing = FALSE)
  assert_var_name(sf_object, var_name)
  
  # change projection to UTM
  sf_object <- suppressWarnings(sf::st_transform(sf_object, 3857))
  

  ###### observed Location Coefficient -----------------------------------------------------
  
  # normalize distribution of variable
  var_x_norm <- normalize_distribution(sf_object[var_name][[1]])
  
  # location coefficient
  LC <- location_coef(var_x_norm)
  
  
  
  ###### observed Venables -----------------------------------------------------
  
  # observed values
  sf_object_observed <- sf_object[sf_object[var_name][[1]] >0, var_name]
  # plot(sf_object_observed['jobs'])
  
  # normalize distribution of variable
  var_x_norm <- normalize_distribution(sf_object_observed[var_name][[1]])
  
  # calculate distance matrix
  distance <- get_distance_matrix(sf_object_observed)
  
  # Spatial separation index (venables)
  v_observed <- venables(var_x_norm, distance)
  
  rm(sf_object_observed, distance, var_x_norm)
  
  
  ###### Find MAX venables spatial separation ------------------------------------
  
  # get perimeter of whole area
  boundary <- sf::st_union(sf_object) |> sf::st_boundary()
  
  # Determine which polygons are on the border
  int <- sf::st_intersects(sf_object, boundary)
  border <- sapply(1:length(int),function(i){length(int[[i]])})
  sf_object$border <- border
  # plot(sf_object['border'])
  
  # keep only cells on the border
  sf_border <- subset(sf_object, border ==1)
  # plot(sf_border['border'])
  
  # calculate distance matrix
  distance_border <- get_distance_matrix(sf_border)
  
  ### HEURISTIC max venables considering full border
  if(isTRUE(full_border)) {
    
    b_full_border <- simulate_border_config(
      sf_object = sf_border,
      nbc = 1, # nbc does not matter
      output = 'vector',
      full_border = full_border
    )
    max_venables <- venables(b = b_full_border, distance = distance_border)
  }
  
  ### BOOSTRAP simulations to find max venables
  if (isFALSE(full_border)) {
    
    # input for number of simulations
    number_busy_cells <- 2:51
    all_sim_input <- rep(number_busy_cells, 400)
    
    # linear
    # if (isFALSE(parallel)) {
      
      # if progress == FALSE, pboptions(type = "none")
      set.seed(42)
      all_sim <- pbapply::pblapply(
        X = all_sim_input,
        FUN = simulate_border_config,
        sf_object = sf_border,
        output = 'vector',
        full_border = full_border
      )
    # }
    
    # # parallel
    # if (isTRUE(parallel)) {
    #   future::plan(future::multisession)
    #   options(future.globals.maxSize = 891289600) # 850 MB
    #   all_sim <- furrr::future_map(
    #     .x = all_sim_input,
    #     .f = simulate_border_config,
    #     sf_object = sf_border,
    #     output = 'vector',
    #     full_border = full_border,
    #     .progress = TRUE,
    #     .options = furrr_options(seed = 42)
    #   )
    # }
    
    # calculate venables of all simulations and get the max value
    
    # linear
    # if (isFALSE(parallel)) {
      all_sim_venables <- pbapply::pblapply(X = all_sim,
                                            FUN = venables,
                                            distance = distance_border)
    # }
    
    # # parallel
    # if (isTRUE(parallel)) {
    #   all_sim_venables <- furrr::future_map(.x = all_sim,
    #                                         .f = venables,
    #                                         distance = distance_border,
    #                                         .progress = TRUE)
    # }
    
    # get max venables spatial separation value from all simulations
    all_sim_venables <- unlist(all_sim_venables)
    max_venables <- max(all_sim_venables)
  }
  
  
  
  
  ###### Calculate UCI -----------------------------------------------------
  
  # Proximity Index PI
  proximity_idex <- 1-(v_observed/max_venables)
  
  # UCI
  UCI <- LC * proximity_idex
  
  output_df <- data.frame(
    UCI = UCI,
    location_coef = LC,
    spatial_separation = v_observed,
    spatial_separation_max = max_venables
  )
  
  return(output_df)
}
  