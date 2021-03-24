####################################
## Animate bus routes
####################################

library(gtfs2gps)
library(data.table)

## read gtfs files
rails <- read_gtfs("data/septa/gtfs_public/google_rail.zip")
buses <- read_gtfs("data/septa/gtfs_public/google_bus.zip")

## filter the data
gtfs_dt <- gtfs2gps::filter_week_days(buses)

gps_dt <- gtfs2gps::gtfs2gps(gtfs_dt, spatial_resolution = 30, parallel = T)
gps_ab <- gps_dt[ between(departure_time, as.ITime("07:00:"), as.ITime("07:31"))]

## create a window arround city hall
zoom <- 
  st_point(c(-75.16357, 39.95259)) %>%
  st_sfc(crs = 4326) %>% 
  st_transform(3857) %>%
  st_buffer(5500) %>%
  st_transform(4326) %>% 
  st_bbox()

mapview::mapview(zoom)

## create a background with census tiger geometries  
tract <- tigris::block_groups('PA', "Philadelphia", class = 'sf')
water <- tigris::area_water('PA', "Philadelphia", class = 'sf') %>%
  st_union() %>% 
  st_combine() 

background <- 
  tract %>% 
  st_union() %>% 
  st_combine() %>% 
  st_difference(water) %>% 
  rmapshaper::ms_simplify(0.05)

## do the same for roads
roads <- tigris::roads("PA", "Philadelphia", class = 'sf')

## identify popular bus stations
hubs <- 
  gps_dt2 %>%
  filter(stop_sequence == 1) %>%
  group_by(shape_pt_lon, shape_pt_lat) %>% 
  summarise(n = n()) %>%
  filter(n > 10) %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)

## call gganimate
library(gganimate)

## animate it
anim <- 
  ggplot() +
  geom_sf(data = background, fill = '#ffffff', colour = NA, alpha = 0.2) +
  geom_sf(data = roads, colour = '#ffffff', size = 0.1) +
  geom_sf(data = hubs, colour = '#ffffff', size = 2) + 
  geom_point(data = gps_dt2 %>% 
              drop_na(stop_sequence), 
             aes(x = shape_pt_lon, y = shape_pt_lat, colour = trip_id), 
             size = 1.5, alpha = 0.75, show.legend = FALSE) +
  scale_colour_manual(values = scico::scico(n = length(unique(gps_dt2$trip_id)), palette = 'hawaii')) + 
  coord_sf(xlim = c(zoom$xmin, zoom$xmax),
           ylim = c(zoom$ymin, zoom$ymax)) +
  labs(title = "Philadelphia Bus Network") +
  theme_black() +
  transition_components(stop_sequence) +  
  shadow_wake(wake_length = 0.015, alpha = FALSE) +
  ease_aes('linear')

## save it
anim_save(animation = anim, "test.gif", height = 800, width = 782)
  


