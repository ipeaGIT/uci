library(sf)
library(ggplot2)
library(h3jsr)


# input is res 10:
get_parent(h3_address = '8abe8d12acaffff', res = 6)
# input is res 6:
get_children(h3_address = '86be8d12fffffff', res = 7)

c1 <- cell_to_polygon('86be8d12fffffff')
c2 <- cell_to_polygon(get_children(h3_address = '86be8d12fffffff',
                                   res = 7)[[1]])
ggplot() +
  geom_sf(data = c1, fill = NA) +
  geom_sf(data = c2,
          fill = 'red', alpha = 0.5 ) +
  theme_void()

cell1 <- st_make_grid(cell1, square=FALSE, cellsize = 10000)
plot(cell1)
   
cell2 <- st_make_grid(cell1, square=FALSE, cellsize = 1000000000)


ggplot() +
  geom_sf(data=cell2, color= 'red') +
  geom_sf(data=cell1) 
  