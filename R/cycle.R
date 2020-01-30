library(tidyverse)
library(fs)

##

files <- dir_ls("data/trips")
length(files)

trips <- map_df(files, ~ read_csv(file = .x, col_types = cols(bike_id = col_character())))

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



