####################################
## Exploring the data
####################################

source("R/package.R")
source("R/help.R")

## identify folder 
files <- dir_ls("data/processed/correlations")

## bind tables
correlations <- map_df(files, vroom)

correlations %>%
  filter(month == 1 & name != "January") %>% 
  mutate(name = factor(name, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
  ggplot(aes(x = name, y = city, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colours = pal,
                       limits = c(0.5, 1), 
                       oob = scales::squish,
                       guide = guide_continuous) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, 
                                   size = 8, hjust = 0.5),
        axis.text.y = element_text(angle = 0, vjust = 0.5,
                                   size = 8, hjust = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_line(size = 0.25), 
        panel.grid.minor = element_line(size = 0.25), 
        legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ggsave("correlations.png", height = 11, width = 17, dpi = 300)

## mapping it
cities <-  
  core_based_statistical_areas(class = 'sf') %>% 
  transmute(metro_fips = GEOID, 
            metro_name = NAME) %>%
  mutate(metro_name = str_replace_all(metro_name, "/", "-"))

information <- 
  read_csv("data/metrolist_two.csv") %>% 
  group_by(metro_name) %>% 
  slice(1) %>% 
  ungroup() %>%
  transmute(metro_name, metro_population, rank)

cities <- 
  cities %>% 
  select(-metro_fips) %>% 
  left_join(information)

states <- 
  states(class = 'sf', cb = TRUE) %>% 
  filter(!str_detect(NAME, "Hawaii|Alaska|Islands|Guam|Samoa|Rico")) %>% 
  ms_simplify(0.005)

## animation
library(gganimate)

animation <- 
  correlations %>%
  filter(month == 1 & name != "January") %>% 
  mutate(name = factor(name, levels = c("February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
  rename(metro_name = city) %>% 
  left_join(cities) %>%
  filter(!str_detect(metro_name, ", PR|, HI")) %>%
  st_as_sf() %>%
  st_centroid() %>%
  st_transform(2163) %>%
  ggplot() +
  geom_sf(data = states, aes(), fill = NA, colour = '#E5E5E3', lwd = 0.5) + 
  geom_sf(aes(colour = value, size = sqrt(metro_population))) +
  scale_colour_gradientn(colours = pal,
                         limits = c(0.7, 1), 
                         oob = scales::squish,
                         guide = guide_continuous) +
  scale_size_continuous(range = c(1, 10), guide = 'none') +
  transition_manual(name) +
  ease_aes() + 
  theme_map() +
  labs(title = "Matrix Correlation between January and {current_frame}") +
  theme(legend.position = 'bottom')

anim_save("correlations.gif", animation = animation, 
          height = 800, width = 1100, fps = 2, 
          start_pause = 0, end_pause = 1) 

## facets
correlations %>%
  filter(month == 1 & name != "January") %>% 
  mutate(name = factor(name, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
  filter(str_detect(name, "April")) %>% 
  rename(metro_name = city) %>% 
  left_join(cities) %>%
  filter(!str_detect(metro_name, ", PR|, HI")) %>%
  st_as_sf() %>%
  st_centroid() %>%
  st_transform(2163) %>%
  ggplot() +
  geom_sf(data = states, aes(), fill = NA, colour = '#E5E5E3', lwd = 0.5) + 
  geom_sf(aes(colour = value, size = sqrt(metro_population))) +
  scale_colour_gradientn(colours = pal,
                         limits = c(0.7, 1), 
                         oob = scales::squish,
                         name = "Matrix Correlation between January and April",
                         guide = guide_continuous) +
  scale_size_continuous(range = c(1, 10), guide = 'none') +
  facet_wrap(~ name) +
  theme_map() +
  theme(legend.position = 'bottom') +
  ggsave("correlations_april.png", height = 8, width = 11, dpi = 300)

## arrange by latitude
coordinates <-
  cities %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>%
  bind_cols(cities) %>% 
  select(metro_name, X, Y)

correlations %>%
  filter(month == 1 & name != "January") %>% 
  mutate(name = factor(name, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
  mutate(metro_name = city) %>% 
  left_join(coordinates) %>%
  filter(!str_detect(metro_name, ", PR|, HI")) %>%
  ggplot(aes(x = name, y = reorder(city, Y), fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colours = pal,
                       limits = c(0.5, 1), 
                       oob = scales::squish,
                       name = "Intracity Matrix Correlation",
                       guide = guide_continuous) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, 
                                   size = 8, hjust = 0.5),
        axis.text.y = element_text(angle = 0, vjust = 0.5,
                                   size = 8, hjust = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_line(size = 0.25), 
        panel.grid.minor = element_line(size = 0.25), 
        legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ggsave("correlations_tiled.png", height = 11, width = 17, dpi = 300)

## identify folder 
files <- dir_ls("data/processed/centralities")

## new palette for sample cities
temp <- sample(pal, 5)

## bind tables
centralities <- map_df(files, ~vroom(.x, col_types = cols(GEOID = col_character())))

## plot time series
centralities %>%
  group_by(city, month) %>%
  summarise(degree = mean(deg)) %>%
  ggplot(aes(x = lubridate::month(month, label = TRUE), y = degree, group = city)) +
  geom_line(colour = '#E5E5E3', size = 1) +
  geom_line(data = centralities %>%
              group_by(city, month) %>%
              summarise(degree = mean(deg)) %>%
              filter(str_detect(city, "New York|San Francisco|Houston|Boston|Phoenix")),
            aes(x = lubridate::month(month, label = TRUE), y = degree, colour = city), size = 1) + 
  scale_colour_manual(values = temp,
                      name = "Mean Degree Centrality") + 
  labs(x = "", y = "") +
  theme_hor() +
  theme(legend.position = c(0.7, 0.85)) +
  ggsave("centralities_series.png", height = 8, width = 11, dpi = 300)

## bind tables
diversities <- map_df(dir_ls("data/processed/diversities"), ~vroom(.x, col_types = cols(GEOID = col_character())))

## plot time series
diversities %>%
  ggplot(aes(x = lubridate::month(month, label = TRUE), y = density, group = city)) +
  geom_line(colour = '#E5E5E3', size = 1) +
  geom_line(data = diversities %>%
              filter(str_detect(city, "New York|San Francisco|Houston|Boston|Phoenix")),
            aes(x = lubridate::month(month, label = TRUE), y = density, colour = city), size = 1) + 
  scale_colour_manual(values = temp,
                      name = "Network Density") + 
  labs(x = "", y = "") +
  theme_hor() +
  theme(legend.position = c(0.7, 0.85)) +
  ggsave("density_series.png", height = 8, width = 11, dpi = 300)

## entropy
diversities %>%
  ggplot(aes(x = lubridate::month(month, label = TRUE), y = entropy, group = city)) +
  geom_line(colour = '#E5E5E3', size = 1) +
  geom_line(data = diversities %>%
              filter(str_detect(city, "New York|San Francisco|Houston|Boston|Phoenix")),
            aes(x = lubridate::month(month, label = TRUE), y = entropy, colour = city), size = 1) + 
  scale_colour_manual(values = temp,
                      name = "Network Entropy") + 
  labs(x = "", y = "") +
  theme_hor() +
  theme(legend.position = c(0.7, 0.3)) +
  ggsave("density_entropy.png", height = 8, width = 11, dpi = 300)

## community size
diversities %>%
  ggplot(aes(x = lubridate::month(month, label = TRUE), y = infomap, group = city)) +
  geom_line(colour = '#E5E5E3', size = 1) +
  geom_line(data = diversities %>%
              filter(str_detect(city, "New York|San Francisco|Houston|Boston|Phoenix")),
            aes(x = lubridate::month(month, label = TRUE), y = infomap, colour = city), size = 1) + 
  scale_colour_manual(values = temp,
                      name = "InfoMap Community Size") + 
  labs(x = "", y = "") +
  theme_hor() +
  theme(legend.position = c(0.7, 0.85)) +
  ggsave("community_series.png", height = 8, width = 11, dpi = 300)
