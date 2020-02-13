library(tidyverse)
library(fs)

##

files <- dir_ls("data/trips")
length(files)

trips <-
  map_df(files, ~ read_csv(file = .x, col_types = cols(bike_id = col_character()))) %>%
  drop_na(start_lat, start_lon, end_lat, end_lon) %>%
  filter(start_station != end_station)

##

glimpse(trips)

##

trips %>% 
  distinct(start_station) %>% 
  dim()

trips %>% 
  distinct(end_station) %>% 
  dim()

##

nodes <-
  trips %>%
  select(trip_id, start_station, end_station) %>%
  drop_na() %>%
  gather(position, station, start_station, end_station) %>%
  distinct(station) %>%
  as_tibble()

edges <-
  trips %>%
  filter(start_station != end_station) %>%
  group_by(start_station, end_station) %>%
  summarise(n = n()) %>%
  drop_na() %>%
  rename(from = start_station,
         to = end_station) %>%
  filter(n > 10)

##

library(igraph)

##

graph <- graph_from_data_frame(edges, vertices = filter(nodes, station %in% edges$from | station %in% edges$to), directed = FALSE)

graph <-
  graph %>%
  set_edge_attr("weight", value = edges$n)

plot(graph,
     vertex.size = 0.5,
     vertex.label = '', 
     edge.width = E(graph)$weight / 100,
     alpha = 0.5,
     layout = layout_with_fr)

##

stations <-
  trips %>%
  distinct(start_station, .keep_all = TRUE) %>%
  transmute(station_id = start_station,
            lat = start_lat,
            lon = start_lon) %>%
  drop_na()

trips_tidy <- 
  trips %>%
  filter(start_station != end_station) %>%
  select(trip_id, start_station, end_station) %>%
  gather(position, station_id, start_station, end_station) %>%
  mutate(position = str_remove_all(position, "_station")) %>%
  left_join(stations)

## TWO 
# Roads as edges
# (Connections)
# Think: M25 | Perifique | West Side Highway  

cleaned <- st_read("data/cleaned.shp")

## 

sf_to_tidygraph = function(x, directed = TRUE) {
  
  edges <- 
    x %>%
    mutate(EDGEID = c(1:n()))
  
  nodes <- 
    edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(EDGEID = L1) %>%
    group_by(EDGEID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    mutate(NODEID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    select(-xy)
  
  source_nodes <- 
    nodes %>%
    filter(start_end == 'start') %>%
    pull(NODEID)
  
  target_nodes <- 
    nodes %>%
    filter(start_end == 'end') %>%
    pull(NODEID)
  
  edges = 
    edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  nodes <- 
    nodes %>%
    distinct(NODEID, .keep_all = TRUE) %>%
    select(-c(EDGEID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  
}

inverted_graph <- sf_to_tidygraph(cleaned, directed = FALSE)

##

inverted_network <- 
  inverted_graph %>%
  activate(edges) %>%
  filter(from != to) %>%
  mutate(length = st_length(geometry))

##

ggplot() +
  geom_sf(data = inverted_network %>% activate(edges) %>% as_tibble() %>% st_as_sf(), size = 0.1) + 
  geom_sf(data = inverted_network %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), size = 0.1) +
  theme_void()

##

inverted_network <- 
  inverted_network %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length))

##

distances <- 
  distances(graph = inverted_network,
            weights = inverted_network %>% activate(edges) %>% pull(length))

##

intersections <- 
  inverted_network %>% 
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_coordinates() %>%
  as_tibble()

##

library(RANN)

##

nn <- nn2(intersections, select(stations, lon, lat), k = 1, searchtype = "radius", radius = 10)

nearest_intersection <-
  stations %>%
  mutate(NODEID = nn$nn.idx)

nearest_intersection <-
  inverted_network %>% 
  activate(nodes) %>%
  as_tibble() %>%
  right_join(nearest_intersection) %>%
  select(NODEID, station_id, lat, lon, degree, betweenness, geometry)

!nearest_intersection$station_id %in% stations$station_id

##

library(lubridate)

##

chunk <- 
  trips %>%
  mutate(week = week(start_time),
         day = wday(start_time, label = TRUE)) %>%
  filter(week == 26 & day == "Mon") %>%
  select(-week, day)

##

routes <- tbl_graph()

##

for (i in 1:nrow(chunk)) {
  
  start <- chunk[[i, "start_station"]]
  end   <- chunk[[i, "end_station"]]
  
  from_node <- 
    nearest_intersection %>%
    filter(station_id == start) %>%
    pull(NODEID)
  
  to_node <- 
    nearest_intersection %>%
    filter(station_id == end) %>%
    pull(NODEID)
  
  path <- 
    shortest_paths(graph = inverted_network,
                   from = from_node,
                   to = to_node,
                   output = 'both',
                   weights = inverted_network %>% activate(edges) %>% pull(length))
  
  path_graph <- 
    inverted_network %>%
    subgraph.edges(eids = path$epath %>% unlist()) %>%
    as_tbl_graph()
  
  path_graph <-
    path_graph %>%
    activate(edges) %>%
    mutate(length = path_graph %>%
             activate(edges) %>%
             as_tibble() %>%
             summarise(length = sum(length)) %>%
             pull(length))
  
  routes <- bind_graphs(routes, path_graph)
  
}

##

guide_discrete <-
  guide_legend(direction = "vertical",
               keywidth = unit(1, units = "mm"),
               keyheight = unit(10, units = "mm"),
               title.position = 'right',
               label.position = 'left',
               title.hjust = 0.5,
               label.hjust = 1,
               ncol = 1,
               bycol = TRUE)

theme_bm_legend <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(colour = 'grey50', angle = 270),
          legend.text = element_text(colour = 'white', angle = 270),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          plot.margin = margin(10, 10, 10, 10),
          legend.position = c(0.8, 0.2)
    )
  
}

##

ggplot() +
  geom_sf(data = inverted_network %>% activate(edges) %>% as_tibble() %>% st_as_sf(crs = projection), 
          col = '#ffffff', size = 0.05, alpha = 0.5) +
  geom_sf(data = routes %>% activate(edges) %>% as_tibble() %>% st_as_sf(crs = projection), 
          aes(colour = factor(ntile(length, 9)), fill = factor(ntile(length, 9)), size = length)) +
  scale_colour_manual(values = pal,
                      labels = str_sub(as.character(quantile(routes %>% activate(edges) %>% as_tibble() %>% pull(length),
                                                             c(0.1,0.2,0.3,0.4,0.5,0.6,0.7, 0.8, 0.9),
                                                             na.rm = TRUE)), 1, 4),
                      guide = guide_discrete,
                      name = "distance") +
    scale_fill_manual(values = pal,
                        labels = str_sub(as.character(quantile(routes %>% activate(edges) %>% as_tibble() %>% pull(length),
                                                               c(0.1,0.2,0.3,0.4,0.5,0.6,0.7, 0.8, 0.9),
                                                               na.rm = TRUE)), 1, 4),
                        guide = guide_discrete,
                        name = "distance") +
  scale_size_continuous(range = c(0.05, 0.5), guide = 'none') +
  labs(title = "PHILADELPHIA CYCLE HIRES", subtitle = "shortest paths between stations") +
  theme_bm_legend() +
  ggsave("test.png", height = 11.1, width = 10, dpi = 300)

##



