library(uci)

data_dir <- system.file("extdata", package = "uci")
grid <- readRDS(file.path(data_dir, "grid_bho.rds"))


system.time(
  df_l <- uci(
    sf_object = grid,
    var_name = 'jobs',
    dist_type = "spatial_link",
    bootstrap_border = FALSE
  )
)

system.time(
  df_e <- uci(
    sf_object = grid,
    var_name = 'jobs',
    dist_type = "euclidean",
    bootstrap_border = FALSE
  )
)









sal <- aopdata::read_landuse(city = 'poa', geometry = T)
plot(sal['T001'])

system.time(
  df_l <- uci(
    sf_object = sal,
    var_name = 'T001',
    dist_type = "spatial_link",
    bootstrap_border = T
  )
)
# 0.5143592     

system.time(
  df_e <- uci(
    sf_object = sal,
    var_name = 'T001',
    dist_type = "euclidean",
    bootstrap_border = T
  )
)
# 0.5029455

### belem-----------------



bel <- aopdata::read_landuse(city = 'bel', geometry = T)
plot(bel['T001'])

system.time(
  df_l <- uci(
    sf_object = bel,
    var_name = 'T001',
    dist_type = "spatial_link",
    bootstrap_border = F
  )
)
# 0.5143592     

sfdep:::check_polygon()

system.time(
  df_e <- uci(
    sf_object = bel,
    var_name = 'T001',
    dist_type = "euclidean",
    bootstrap_border = T
  )
)
