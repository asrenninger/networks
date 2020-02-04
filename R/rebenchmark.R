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

projection[1]
projection[2]

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

##

tic()

difference <- st_difference(roads)
intersection <- st_intersection(roads)

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

join <- 
  nodes %>% 
  st_join(difference) %>%
  clean_names()

##

tic()

verts <-
  join %>%
  drop_na() %>%
  gather(variable, value, linearid_x, linearid_y) %>%
  use_series(value) %>%
  unique() %>%
  as_tibble() %>%
  rename(id = value)

toc()

##

tic()

verts <-
  join %>%
  drop_na() %>%
  st_drop_geometry() %>%
  gather(variable, value, linearid_x, linearid_y) %>%
  distinct(value) %>%
  rename(id = value)

toc()

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

graph <- as_tbl_graph(graph)

##

graph <-
  graph %>%
  activate(nodes) %>%
  mutate(LINEARID = name) %>%
  left_join(roads)

##

ggplot() +
  geom_sf(data =
            graph %>% 
            activate(nodes) %>%
            as_tibble() %>%
            st_as_sf(),
          aes(), size = 0.2) +
  geom_sf(data =
            graph %>%
            activate(edges) %>%
            as_tibble() %>%
            st_as_sf() %>%
            st_set_crs(projection),
          aes(), size = 0.5) +
  theme_void()

##


