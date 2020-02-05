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

edges <- 
  roads %>%
  mutate(EDGEID = c(1:n()))

nodes <- 
  edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(EDGEID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

##

clean_sf = function(x) {
  
  duplicates <- 
    x %>% 
    st_equals() %>%
    as_tibble() %>%
    rename(original = row.id,
           duplicate = col.id) %>%
    group_by(original) %>%
    mutate(grouping = max(duplicate)) %>%
    ungroup() %>%
    distinct(grouping)
  
  roads <- 
    x %>% 
    slice(duplicates$grouping)
  
  containers <- 
    roads %>% 
    st_contains() %>%
    as_tibble() %>%
    rename(original = row.id,
           duplicate = col.id) %>%
    filter(original != duplicate) 
  
  roads <-
    roads %>%
    slice(-unique(containers$duplicate))
  
  
  roads %>%
    mutate(type = st_geometry_type(geometry)) %>%
    filter(type == "LINESTRING") %>%
    select(-type)
  
}

cleaned <- clean_sf(roads)

##

sf_to_tidygraph = function(x, directed = TRUE) {
  
  edges <- 
    x %>%
    mutate(EDGEID = c(1:n()))
  
  nodes <-
    st_intersection(edges) %>%
    mutate(type = st_geometry_type(geometry)) %>%
    filter(type == "POINT") %>%
    select(-type) %>%
    st_as_sf()
  
  nodes <- 
    nodes %>%
    st_join(edges) %>%
    select(EDGEID.x, EDGEID.y) %>%
    rename(start = EDGEID.x,
           end = EDGEID.y) %>%
    gather(start_end, EDGEID, start:end)
  
  source_nodes <- 
    nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  
}

regraph <- sf_to_tidygraph(roads, directed = FALSE)

##

regraph <- 
  regraph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

regraph

##

regraph %>%
  activate(edges) %>%
  as_tibble() %>%
  st_as_sf() %>%
  group_by(RTTYP) %>%
  summarise(length = sum(length))

##

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(crs = projection)) + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(crs = projection), size = 0.5)
