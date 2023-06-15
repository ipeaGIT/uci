# uci (development version)

# uci 0.2.0

## New features

- The `uci` function now has a new parameter `dist_type` that allows users to choose  whether calculations should be based on `"euclidean"` distances (Default) or `"spatial_link"` distances. Spatial link distances consider Euclidean distances along the links of spatial neighbour links. In the case of areas with a concave shape (like a bay), it is strongly recommended to use `"spatial_link"` distances (even though they are computationally more costly) because simple Euclidean distances can bias UCI estimates in those cases.
- The `uci` function now has a new parameter `parallel` that speeds up calculations for large areas comprised with many polygons.

## Notes

- Minor updates to documentation
- Performance improvments


# uci 0.1.0

- Initial CRAN release.
