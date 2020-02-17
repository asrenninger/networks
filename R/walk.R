library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

##

roads <- roads("PA", "Philadelphia", class = 'sf')
tracts <- tracts("PA", "Philadelphia", class = 'sf')

water <- 
  area_water("PA", "Philadelphia", class = 'sf') %>%
  st_union() %>%
  st_combine()

##

projection <- st_crs(roads)

##

library(tidyverse)

##

background <-
  tracts %>%
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise() %>%
  st_difference(water)

##

ggplot() +
  geom_sf(data = background,
          aes(), fill = '#7d7d7d', colour = NA) +
  geom_sf(data = roads,
          aes(), fill = NA, colour = '#ffffff', alpha = 0.5, size = 0.1) +
  theme_bm_legend()

## PROGRAM
# Two appraoches
# Roads as nodes
# Roads as edges
# + / - for each

## ONE 
# Roads as nodes
# (Places to be)
# Think: Oxford Street | Champs Elysee | 5th Avenue  

library(tictoc)

##

tic()

difference <- st_difference(roads)
intersection <- st_intersection(difference)

toc()

##

nodes <-
  intersection %>%
  mutate(type = st_geometry_type(geometry)) %>%
  filter(type == "POINT") %>%
  select(-type) %>%
  st_as_sf()

##

library(janitor)

##

join <- 
  nodes %>% 
  st_join(intersection) %>%
  clean_names()

##

verts <-
  join %>%
  drop_na() %>%
  st_drop_geometry() %>%
  gather(variable, value, linearid_x, linearid_y) %>%
  distinct(value) %>%
  rename(id = value)

##

links <-
  join %>%
  drop_na() %>%
  filter(linearid_x != linearid_y) %>%
  select(linearid_x, linearid_y) %>%
  rename(from = linearid_x,
         to = linearid_y)

##

library(igraph)

##

graph <- graph_from_data_frame(links, vertices = verts, directed = FALSE)

plot(graph,
     vertex.size = 0.1,
     vertex.label = '', 
     alpha = 0.5)

##

library(tidygraph)

##

graph <- as_tbl_graph(graph)

##

graph <-
  graph %>%
  activate(nodes) %>%
  mutate(LINEARID = name) %>%
  left_join(roads)

graph <-
  graph %>%
  activate(nodes) %>%
  mutate(length = st_length(geometry))

##

ggplot() +
  geom_sf(data =
            graph %>% 
            activate(nodes) %>%
            as_tibble() %>%
            st_as_sf(),
          aes(colour = as.numeric(length)), size = 0.5) +
  geom_sf(data =
            graph %>%
            activate(edges) %>%
            as_tibble() %>%
            st_as_sf(crs = projection),
          aes(), size = 0.3) +
  scale_colour_gradientn(colours = pal, guide = 'none') +
  theme_void()

##

lines <- 
  graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf(crs = projection)

points <-
  graph %>%
  activate(edges) %>%
  as_tibble() %>%
  st_as_sf(crs = projection)

##

st_write(lines, "lines.shp")
st_write(points, "points.shp")

##