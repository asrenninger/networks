library(geofacet)
library(geogrid)
library(tidyverse)
library(lubridate)
library(sf)
library(janitor)

baseline <-
  bind_rows(read_csv("data/indego/trips/indego-trips-2019-q1.csv.zip"),
            read_csv("data/indego/trips/indego-trips-2018-q1.csv.zip")) %>%
  clean_names() %>%
  mutate(time = floor_date(start_time, "day"),
         station_id = start_station) %>%
  filter(station_id != 3000) %>%
  group_by(station_id, time) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(day = yday(time)) %>%
  group_by(station_id, day) %>%
  summarise(baseline = mean(n, na.rm = TRUE))

stations <- 
  read_csv("data/indego/indego-stations-2020-04-01.csv") %>%
  clean_names()

trips <- 
  read_csv("data/indego/trips/indego-trips-2020-q1.csv.zip") %>%
  clean_names()

points <-
  trips %>% 
  transmute(lat = start_lat,
            lon = start_lon, 
            station = start_station) %>%
  distinct(station, .keep_all = TRUE) %>%
  drop_na(lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(3702) %>%
  st_coordinates() %>% 
  as_tibble()

info <- 
  trips %>%
  select(start_lat, start_lon, start_station) %>%
  filter(start_station != 3000) %>%
  distinct(start_station, .keep_all = TRUE) %>%
  rename(lat = start_lat,
         lon = start_lon,
         station_id = start_station) %>%
  left_join(stations) %>%
  bind_cols(points) %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) %>%
  st_transform(3702)

plot(info)

library(deldir)
library(spatstat)
library(maptools)

buffer <- st_buffer(info, 250)

dmat <- st_distance(buffer)
hc <- hclust(as.dist(dmat > units::set_units(1, m)), method = "single")
groups <- cutree(hc, h = 0.5)

length(groups)
length(unique(groups))

clusters <-
  buffer %>%
  mutate(group = groups) %>%
  group_by(group) %>%
  summarise() %>%
  rownames_to_column()

points <-
  clusters %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble()

pattern <- as.ppp(points, W = ripras(points, shape = "rectangle")) 
shape <- dirichlet(pattern)

voronoi <- 
  shape %>%
  as("SpatialPolygons") %>% 
  st_as_sf() %>%
  st_set_crs(3702) %>%
  st_join(st_centroid(clusters))

plot(voronoi)

bounds <- 
  clusters %>%
  st_buffer(1000) %>%
  st_union()

gridata <- 
  voronoi %>%
  st_intersection(bounds)

plot(gridata)

new_cells_hex <- calculate_grid(shape = gridata, grid_type = "regular", 
                                learning_rate = 0.03, seed = 42, verbose = TRUE)

resulthex <- assign_polygons(gridata, new_cells_hex)

plot(resulthex)

network <-
  info %>%
  mutate(group = groups) %>%
  select(group, station_id) %>%
  st_drop_geometry() %>%
  group_by(group) %>%
  summarise(stations = split(station_id, group))

final <- 
  resulthex %>%
  transmute(col = dense_rank(V1),
            row = dense_rank(-V2),
            group = group) %>%
  left_join(network)

grid <- 
  final %>%
  st_drop_geometry() %>%
  rename(code = group, 
         name = stations) %>%
  mutate(code = factor(code)) %>%
  as_tibble()

grid_preview(grid)
grid_submit(grid)

panel <-
  trips %>%
  mutate(start_time = str_replace_all(start_time, pattern = "/", replacement = "-"),
         end_time = str_replace_all(end_time, pattern = "/", replacement = "-")) %>%
  mutate(start_time = mdy_hm(start_time),
         end_time = mdy_hm(end_time)) %>%
  mutate(time = floor_date(start_time, "day"),
         station_id = start_station) %>%
  filter(station_id != 3000) %>%
  group_by(station_id, time) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(day = yday(time)) %>%
  select(day, station_id, n) %>%
  left_join(baseline)

crosswalk <-
  info %>%
  st_drop_geometry() %>%
  select(station_id, station_name) %>%
  mutate(group = groups) %>%
  mutate(code = factor(group))

theme_hor <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, colour = 'black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, colour = 'black'),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
}

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/turbo.txt", col_names = FALSE) %>% pull(X1)

ggplot(panel %>%
         left_join(crosswalk) %>%
         replace_na(list(baseline = 0)) %>%
         group_by(code, day) %>%
         summarise(n = mean(n, na.rm = TRUE),
                   baseline = mean(baseline, na.rm = TRUE)),
       aes(x = day, y = n - baseline, colour = code)) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous(breaks = c(20, 0, -20)) +
  scale_colour_manual(values = sample(pal, 50, replace = TRUE)) +
  facet_geo(~ code, grid = grid) +
  theme_hor() +
  ggsave("grid.png", height = 8, width = 8, dpi = 300)


