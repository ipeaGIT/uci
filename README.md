# uci: Urban Centrality Index

**uci** is an `R` package to calculate the Urban Centrality Index (UCI) originally proposed by Pereira et al., (2013). The UCI measures the extent to which the spatial organization of a city or region varies from extreme monocentric to extreme polycentric in a continuous scale from 0 to 1. Values close to 0 indicate more polycentric patterns and values close to 1 indicate a more monocentric urban form.

- Pereira, R. H. M., Nadalin, V., Monasterio, L., & Albuquerque, P. H. (2013). **Urban centrality: a simple index**. *Geographical analysis*, 45(1), 77-89. [https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12002](https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12002)
  - [Link to ungated PDF](https://www.urbandemographics.org/publication/2013_urban_centrality_index/)



## Installation

For now, you can install the dev version of `uci`:

```R
utils::remove.packages('uci')
devtools::install_github("rafapereirabr/uci")
library(uci)

```



## Basic Usage

```R
# load data
data_dir <- system.file("extdata", package = "uci")
grid <- readRDS(file.path(data_dir, "grid_bho.rds"))

head(grid)
#> Simple feature collection with 6 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -43.96438 ymin: -19.97414 xmax: -43.93284 ymax: -19.96717
#> Geodetic CRS:  WGS 84
#>                id population jobs schools                       geometry
#> 1 89a881a5a2bffff        439  180       0 POLYGON ((-43.9431 -19.9741...
#> 2 89a881a5a2fffff        266  134       0 POLYGON ((-43.94612 -19.972...
#> 3 89a881a5a67ffff       1069  143       0 POLYGON ((-43.94001 -19.972...
#> 4 89a881a5a6bffff        245   61       0 POLYGON ((-43.9339 -19.9728...
#> 5 89a881a5a6fffff        298   11       0 POLYGON ((-43.93691 -19.971...
#> 6 89a881a5b03ffff        555 1071       0 POLYGON ((-43.96136 -19.970...

# calculate UCI
df <- uci(
       sf_object = grid,
       var_name = 'jobs',
       boostrap_border = FALSE
       )

head(df)
#>         UCI location_coef spatial_separation spatial_separation_max
#> 1 0.2538635     0.5278007           3880.114               7475.899

```
