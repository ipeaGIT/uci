library(uci)
library(aopdata)

data_dir <- system.file("extdata", package = "uci")
grid <- readRDS(file.path(data_dir, "grid_bho.rds"))


system.time(
  df_l <- uci(
    sf_object = grid,
    var_name = 'jobs',
    dist_type = "euclidean",
    bootstrap_border = T,
    showProgress = T, 
    parallel = T

  )
)

system.time(
  df_e <- uci(
    sf_object = grid,
    var_name = 'jobs',
    dist_type = "euclidean",
    bootstrap_border = F, 
    showProgress = T, 
    parallel = T
  )
)


system.time(
  df_e <- uci(
    sf_object = grid,
    var_name = 'jobs',
    dist_type = "euclidean",
    bootstrap_border = FALSE,  parallel = F
  )
)






city <- aopdata::read_landuse(city = 'spo', geometry = T)
plot(city['T001'])

system.time(
  df_l <- uci(
    sf_object = city,
    var_name = 'T001',
    dist_type = "euclidean",
    bootstrap_border = T,
    showProgress = T, 
    parallel = T
    
  )
)

# soh parte 2 parallel 42.20
# tudo parallel  40.00

system.time(
  df_e <- uci(
    sf_object = city,
    var_name = 'T001',
    dist_type = "euclidean",
    bootstrap_border = F, 
    showProgress = T, 
    parallel = T
  )
)
# soh parte 2 parallel 11
# tudo parallel  12.00
