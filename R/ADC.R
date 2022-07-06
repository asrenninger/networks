###################################
## ADC
###################################

## packages
source("R/package.R")
source("R/help.R")
source("R/rehelp.R")
source("R/query.R")

## should we use mean or median as centre? 
library(tmap)
tmap_mode("view")

clusters <- st_as_sf(clusters, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

tm_shape(clusters) +
  tm_dots() +
  tm_shape(st_sfc(st_point(c(mean(clusters$longitude), mean(clusters$latitude))), crs = 4326)) +
  tm_dots(col = "red") + 
  tm_shape(st_sfc(st_point(c(median(clusters$longitude), median(clusters$latitude))), crs = 4326)) +
  tm_dots(col = "blue") 

## blocks
options(tigris_use_cache = TRUE)

shape <- 
  reduce(
    map(codes %>% 
          str_remove_all("\'") %>% 
          str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          block_groups(state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), class = 'sf')
        }),
    rbind)

## add population
population <- 
  reduce(
    map(codes %>% 
          str_remove_all("\'") %>% 
          str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          tidycensus::get_acs(state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), year = 2019,  
                              variables = "B01001_001", geography = 'block group', geometry = TRUE)
        }),
    rbind) %>%
  filter(estimate != 0)

## remove water to make it more accurate
# water <- 
#   reduce(
#     map(codes %>% 
#           str_remove_all("\'") %>% 
#           str_split(", ") %>% 
#           magrittr::extract2(1),
#         function(x) { 
#           area_water(state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), class = 'sf')
#         }),
#     rbind)
# 
# population <- st_difference(population, water)

## distances
points <- reduce(purrr::map2(population$geometry, population$estimate, ~st_sample(.x, size = .y %/% 100)), c)
centre <- st_sfc(st_point(c(mean(clusters$longitude), mean(clusters$latitude))), crs = 4326)

distances <- 
  points %>% 
  st_as_sf(crs = st_crs(shape)) %>%
  st_transform(2163) %>%
  mutate(distance = st_distance(x, st_transform(centre, 2163)))

# adc 
adc <- mean(units::drop_units(distances$distance))

# norm
boundary <-
  population %>% 
  st_union() %>% 
  st_combine()

null <- 
  boundary %>%
  st_make_grid() %>%
  st_intersection(boundary) %>% 
  st_transform(2163) %>% 
  st_centroid() %>%
  st_distance(st_transform(centre, 2163)) %>% 
  units::drop_units() %>% 
  mean()

adc_norm <- adc / null

