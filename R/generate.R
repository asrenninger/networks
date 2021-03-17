library(tidyverse)
library(sf)

bounds <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
points <- st_sample(bounds, 100) %>% st_coordinates %>% as_tibble()
pointp <- spatstat::ppp(points$X, points$Y, window = spatstat::owin(poly = bounds))
level_1 <- 
  spatstat::dirichlet(pointp) %>% 
  st_as_sfc()

level_2 <- reduce(
  map(1:nrow(level_1), 
      function(x){
        bounds <- level_1$geometry[[x]]
        revved <- bounds %>% st_coordinates() %>% as_tibble()
        points <- st_sample(bounds, 100) %>% st_coordinates %>% as_tibble()
        pointp <- ppp(points$X, points$Y, window = owin(poly = tibble(x = rev(revved$X), y = rev(revved$Y))))
        dirich <- 
          dirichlet(pointp) %>% 
          st_as_sfc()
      }), rbind
) 

level_3 <- reduce(
  map(1:nrow(level_2), 
      function(x){
        bounds <- level_2$geometry[[x]]
        revved <- bounds %>% st_coordinates() %>% as_tibble()
        points <- st_sample(bounds, 10) %>% st_coordinates %>% as_tibble()
        pointp <- ppp(points$X, points$Y, window = owin(poly = tibble(x = rev(revved$X), y = rev(revved$Y))))
        dirich <- 
          dirichlet(pointp) %>% 
          st_as_sfc()
      }), rbind
) 

ggplot() +
  geom_sf(data = level_2, aes(), 
          colour = 'grey50', alpha = 0.5, fill = NA, size = 1.0) +
  geom_sf(data = level_1, aes(), 
          colour = 'grey30', alpha = 0.9, fill = NA, size = 1.5) +
  theme_void() +
  ggsave("test.png", height = 20, width = 20, dpi = 300)

bounds <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
points <- st_sample(bounds, 100) %>% st_coordinates %>% as_tibble()
pointp <- ppp(points$X, points$Y, window = owin(poly = bounds))
level_1 <- 
  dirichlet(pointp) %>% 
  st_as_sfc()

points <- level_1 %>% st_centroid() %>% st_coordinates %>% as_tibble()
pointp <- ppp(points$X, points$Y, window = owin(poly = bounds))
level_2 <- 
  dirichlet(pointp) %>% 
  st_as_sfc()

points <- level_2 %>% st_centroid() %>% st_coordinates %>% as_tibble()
pointp <- ppp(points$X, points$Y, window = owin(poly = bounds))
level_3 <- 
  dirichlet(pointp) %>% 
  st_as_sfc()

ggplot() +
  geom_sf(data = level_3, aes(), 
          colour = 'grey80', alpha = 0.25, fill = NA, size = 0.5) +
  geom_sf(data = level_2, aes(), 
          colour = 'grey50', alpha = 0.50, fill = NA, size = 1.0) +
  geom_sf(data = level_1, aes(), 
          colour = 'grey30', alpha = 0.75, fill = NA, size = 1.5) +
  theme_void() +
  ggsave("test2.png", height = 20, width = 20, dpi = 300)

####################################
## Lloyd's Algorithm
####################################

## create a bounding box
square <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))

## generatea regular tesselation from it
bounds <- 
  square %>% 
  st_make_grid(n = c(25, 25), square = FALSE) %>% 
  st_union() %>% 
  st_combine()

## cast to polygon
bounds <- st_cast(new_bounds, 'POLYGON')

## build a data frame of coordinates for spatstat
framed <-
  bounds %>% 
  st_coordinates() %>% 
  as_tibble()

## generate points from a gaussian kernel so they start close to the middle and need to sort themselves out
points <- tibble(X = rnorm(n = 25 * 25, mean = 0, sd = 0.1), Y = rnorm(n = 25 * 25, mean = 0, sd = 0.1))

## generate point pattern from these points
pointp <- spatstat::ppp(points$X, points$Y, window = spatstat::owin(poly = tibble(x = rev(framed$X), y = rev(framed$Y))))

## create the voronoi tesselation from this pattern
dirich <- 
  spatstat::dirichlet(pointp) %>% 
  st_as_sfc() %>% 
  st_as_sf() %>%
  mutate(iteration = 1) %>%
  rename(geometry = x) %>%
  select(iteration, geometry)

## plot to be sure it worked
plot(dirich)

## iterate through with Lloyd's Algorithm
for (i in 1:500) {
  
  ## create a temporary data frame
  temporary <- filter(dirich, iteration == i)
  
  ## calculate the centroids of those voronoi polygons
  temporary_points <- 
    temporary %>% 
    st_centroid() %>% 
    st_coordinates %>% 
    as_tibble()
  
  ## create a new point pattern
  temporary_pointp <- spatstat::ppp(temporary_points$X, temporary_points$Y, 
                                    window = spatstat::owin(poly = tibble(x = rev(framed$X), y = rev(framed$Y))))  
  
  ## calculate a new voronoi tesselation
  temporary_dirich <- 
    spatstat::dirichlet(temporary_pointp) %>% 
    st_as_sfc() %>% 
    st_as_sf() %>%
    mutate(iteration = i + 1) %>%
    rename(geometry = x) %>%
    select(iteration, geometry)
  
  ## bind it to the original so that we can see the iterations 
  dirich <- rbind(dirich, temporary_dirich)
  
}

library(gganimate)

## calculate the area of the ideal cell for comparison
regular <- 
  bounds %>%
  st_make_grid(n = c(25, 25), square = FALSE) %>% 
  magrittr::extract2(1) %>% 
  st_area()

## animate it
anim <- 
  ggplot(new_dirich %>% 
           mutate(area = st_area(geometry),
                  diff = (area - regular) / regular)) + 
  geom_sf(aes(fill = factor(ntile(diff, 9))), colour = '#ffffff', show.legend = FALSE) +
  scale_fill_manual(values = scico::scico(9, palette = 'hawaii')) +
  transition_manual(iteration) +
  theme_void()

anim_save("test.gif", animation = anim, fps = 25, start_pause = 5, end_pause = 10, height = 800, width = 800)

## central place theory
rotate <- function(a) { matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2) }

square <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
grid <- st_make_grid(square, n = c(25, 25), square = FALSE)

neighbours <- grid %>% st_touches(sparse = TRUE) %>% map(~length(.x)) %>% reduce(c)

trimmed <- grid[neighbours > 2]
rotated <- (trimmed * rotate(pi/2))

coordinates <- rotated %>% st_set_precision(0.5) %>% st_centroid %>% st_coordinates() %>% as_tibble() 

trimmed <- rotated[coordinates$X != max(coordinates$X)]
tibbled <- trimmed %>% st_as_sf() %>% rownames_to_column(var = "geocode") %>% rename(geometry = x) %>% st_centroid()

tween <- 
  trimmed %>% 
  st_touches() %>% 
  as.data.frame() %>% 
  graph_from_data_frame() %>%
  betweenness() %>%
  as_tibble() %>%
  rownames_to_column(var = "row_id") %>%
  mutate(row_id = as.numeric(row_id))

plot(trimmed)

trimmed %>% 
  st_touches() %>% 
  as.data.frame() %>% 
  clean_names() %>%
  glimpse() %>% 
  stplanr::od2line(., tibbled) %>% 
  left_join(tween) %>%
  select(value) %>%
  plot(add = TRUE)

edges <- 
  trimmed %>%
  st_cast("LINESTRING") %>%
  lwgeom::st_split(st_cast(trimmed, "POINT")) %>%
  st_collection_extract("LINESTRING")

tween_edges <- 
  edges %>% 
  st_touches() %>% 
  as.data.frame() %>% 
  graph_from_data_frame() %>%
  closeness() %>%
  as_tibble() %>%
  rownames_to_column(var = "row_id") %>%
  mutate(row_id = as.numeric(row_id))

ggplot() +
  geom_sf(data = edges %>%
            as_tibble() %>% 
            rownames_to_column(var = "row_id") %>%
            mutate(row_id = as.numeric(row_id)) %>%
            glimpse() %>% 
            left_join(tween_edges) %>%
            st_as_sf() %>%
            select(value), 
          aes(colour = value, lwd = value),
          show.legend = FALSE) +
  scale_colour_gradientn(colors = pal) + 
  scale_size_continuous(range = c(0.1, 1)) +
  theme_map() +
  ggsave("tessellation.png", height = 8, width = 8, dpi = 300)


ggplot() +
  geom_sf(data = trimmed, 
          aes(),
          fill = NA) + 
  geom_sf(data = trimmed %>% 
            st_touches() %>% 
            as.data.frame() %>% 
            clean_names() %>%
            glimpse() %>% 
            stplanr::od2line(., tibbled) %>% 
            left_join(tween) %>%
            select(value), 
          aes(colour = value, lwd = value), 
          show.legend = FALSE) +
  scale_colour_gradientn(colors = pal) + 
  scale_size_continuous(range = c(0.1, 1)) +
  theme_map() + 
  ggsave("chrystaller.png", height = 8, width = 8, dpi = 300)
