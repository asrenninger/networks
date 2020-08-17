library(geofacet)
library(geogrid)
library(tidyverse)
library(lubridate)
library(sf)
library(janitor)

library(fs)

files <- dir_ls("data/indego/trips")

trips <-
  bind_rows(read_csv("data/indego/trips/indego-trips-2020-q1.csv") %>%
              mutate(start_time = mdy_hm(start_time),
                     bike_id = as.character(bike_id),
                     baseline = "0") %>%
              select(-end_time),
            read_csv("data/indego/trips/indego-trips-2020-q2.csv") %>%
              mutate(start_time = mdy_hm(start_time),
                     bike_id = as.character(bike_id),
                     baseline = "0") %>%
              select(-end_time),
            read_csv("data/indego/trips/indego-trips-2019-q1.csv") %>%
              mutate(bike_id = as.character(bike_id),
                     baseline = "1") %>%
              select(-end_time),
            read_csv("data/indego/trips/indego-trips-2019-q2.csv") %>%
              mutate(bike_id = as.character(bike_id),
                     baseline = "1") %>%
              select(-end_time)) %>%
  clean_names() %>%
  mutate(time = floor_date(start_time, "day"),
         day = yday(time), 
         station_id = start_station) %>%
  filter(station_id != 3000) %>%
  group_by(station_id, start_lat, start_lon, day, baseline) %>%
  summarise(n = n()) %>%
  ungroup()

stations <- 
  read_csv("data/indego/indego-stations-2020-04-01.csv") %>%
  clean_names()

points <-
  trips %>% 
  transmute(lat = start_lat,
            lon = start_lon, 
            station = station_id) %>%
  distinct(station, .keep_all = TRUE) %>%
  drop_na(lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(3702) %>%
  st_coordinates() %>% 
  as_tibble()

info <- 
  trips %>%
  select(start_lat, start_lon, station_id) %>%
  filter(station_id != 3000) %>%
  distinct(station_id, .keep_all = TRUE) %>%
  rename(lat = start_lat,
         lon = start_lon,
         station_id = station_id) %>%
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
        #  axis.text.x = element_blank(),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
}

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/turbo.txt", col_names = FALSE) %>% pull(X1)
set <- sample(pal, 50, replace = TRUE)

ggplot(trips %>%
         left_join(crosswalk) %>%
         replace_na(list(n = 0)) %>%
         pivot_wider(names_from = baseline, values_from = n) %>% 
         clean_names() %>%
         rename(current = x0,
                baseline = x1) %>%
         group_by(code, day) %>%
         summarise(n = mean(current, na.rm = TRUE),
                   baseline = mean(baseline, na.rm = TRUE)) %>%
         mutate(decline = min(n) - mean(n)),
       aes(x = day, y = n - baseline, colour = decline)) +
  geom_line(show.legend = FALSE) +
  #scale_y_continuous(breaks = c(20, 0, -20)) +
  scale_colour_gradientn(colours = rev(pal)) +
  facet_geo(~ code, grid = grid) +
  theme_hor() +
  ggsave("grid.png", height = 12, width = 8, dpi = 300)

trips %>%
  group_by(day, baseline) %>%
  summarise(sum = n()) %>%
  ungroup() %>%
  group_by(baseline) %>%
  mutate(l1 = lag(sum),
         l2 = lag(l1),
         l3 = lag(l2),
         l4 = lag(l3),
         l5 = lag(l4),
         l6 = lag(l5)) %>%
  mutate(rolling = (sum + l1 + l2 + l3 + l4 + l5 + l6) / 7) %>%
  ggplot(aes(day, rolling, colour = factor(baseline))) +
  geom_line()



