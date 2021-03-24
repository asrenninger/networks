########################################
## Spatial network mapping
########################################

source("R/package.R")
source("R/help.R")

##

odmat <- vroom("data/processed/od_monthly.csv") %>% glimpse()
phila <- read_sf("data/processed/phila.geojson") %>% glimpse()

##

businesses <- rename(phila, geocode1 = GEOID)

##

shape <- 
  block_groups("PA", "Philadelphia", cb = TRUE, class = 'sf') %>%
  st_transform(3702)

water <- 
  area_water("PA", "Philadelphia", class = 'sf') %>%
  st_transform(3702) %>%
  st_union() %>%
  st_combine()

background <- 
  shape %>%
  st_union() %>%
  st_combine() %>%
  st_difference(water) %>%
  ms_simplify(0.005)

##

datum <- 
  odmat %>%
  mutate(month = lubridate::month(start, label = TRUE, abbr = FALSE)) %>% 
  filter(safegraph_place_id %in% businesses$safegraph_place_id) %>% 
  left_join(businesses) %>% 
  select(location_name, safegraph_place_id, cbg, visits, month, geocode1) %>%
  rename(GEOID = cbg) %>% 
  left_join(shape) %>%
  drop_na(ALAND, AWATER) %>% 
  st_as_sf()

datum <- 
  datum %>% 
  transmute(geocode1 = geocode1,
            geocode2 = GEOID,
            visits = visits,
            month = month) %>%
  st_drop_geometry() %>%
  group_by(geocode1, geocode2, month) %>%
  summarise(visits = sum(visits)) %>%
  filter(visits > 9)
  
glimpse(datum)
glimpse(shape)

lines <- stplanr::od2line(flow = datum, zones = transmute(shape, geocode = GEOID))

##

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = lines, aes(colour = factor(ntile(visits, 9)), lwd = visits, alpha = visits)) +
  scale_colour_manual(values = rev(pal),
                    labels = as.character(quantile(datum$visits,
                                                   c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                                   na.rm = TRUE)),
                    name = "visits",
                    guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ month, nrow = 2) +
  #labs(title = 'Origin-Destination Flows', subtitle = "Connections between neighborhoods and points of interest") +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "flows_clean.png", height = 12, width = 14, dpi = 300)

##

library(gganimate)

##

anim <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = lines %>%
            mutate(month = fct_drop(month)), aes(colour = factor(ntile(visits, 9)), lwd = visits, alpha = visits)) +
  scale_colour_manual(values = rev(pal),
                      labels = as.character(quantile(datum$visits,
                                                     c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                                     na.rm = TRUE)),
                      name = "visits",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  transition_manual(month) +
  ease_aes() +
  labs(#title = 'Origin-Dstination Flows', 
       subtitle = "{current_frame}") +
  theme_map() +
  theme(legend.position = 'bottom')

anim_save("flows.gif", animation = anim, 
          height = 800, width = 800, nframes = 10, fps = 1, 
          start_pause = 1, end_pause = 1) 
