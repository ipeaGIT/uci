# uci (development version)

## New features

- The `uci` function now has a new parameter `dist_type` that allows users to choose  whether calculations should be based on `"euclidean"` distances (Default) or `"spatial_link"` distances. Spatial link distances consider Euclidean distances along the links of spatial neighbour links. In the case of areas with a concave shape (like a bay), it is strongly recommended to use `"spatial_link"` distances (even though they are computationally more costly) because simple Euclidean distances can bias UCI estimates in those cases.

# uci 0.1.0

- Initial CRAN release.
