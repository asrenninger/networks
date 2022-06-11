###################################
## RDI
###################################

## packages
source("R/package.R")
source("R/help.R")
source("R/rehelp.R")
source("R/query.R")

## load in cities
city_list <- 
  read_csv("data/metrolist_two.csv") %>%
  drop_na(metro_name) %>%
  filter(rank > 0, rank < 102) %>%
  pull(metro_name) %>%
  unique()

## select city
city <- city_list[14]

## fips codes
codes <- get_codes(city)

## block groups 
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

## water areas
water <- 
  reduce(
    map(codes %>% 
          str_remove_all("\'") %>% 
          str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          area_water(state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), class = 'sf')
        }),
    rbind)

## river lines (unnecessary)
river <- 
  reduce(
    map(codes %>% 
          str_remove_all("\'") %>% 
          str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          linear_water(state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), class = 'sf')
        }),
    rbind)

## aggregated blocks
bounds <-
  shape %>% 
  st_transform(3857) %>%
  st_union() %>% 
  st_combine() 

plot(bounds)

## roads
roads <- 
  primary_roads() %>%
  st_transform(3857) %>%
  st_intersection(bounds) %>%
  filter(st_geometry_type(geometry) != "POINT") %>%
  st_union() %>% 
  st_combine()

## rails
rails <-
  rails() %>%
  st_transform(3857) %>%
  st_intersection(bounds) %>%
  filter(st_geometry_type(geometry) != "POINT") %>%
  st_union() %>% 
  st_combine()

## aggregated water
water <-
  water %>%
  st_transform(3857) %>%
  st_union() %>% 
  st_combine()

## plot to check
plot(st_geometry(roads), col = 'green', add = T)
plot(st_geometry(rails), col = 'red', add = T)
plot(st_geometry(water), col = 'blue', add = T)
plot(st_geometry(river), col = 'lightblue', add = T)

## differences out rail and road
no_road <- st_difference(bounds, st_buffer(roads, 20))
no_rail <- st_difference(no_road, st_buffer(rails, 20))

## get counties
counties <- 
  shape %>%
  group_by(GEOID = str_sub(GEOID, 1, 5)) %>%
  summarise()

## difference out water
no_water <- 
  no_rail %>%
  st_difference(water) %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(area = units::drop_units(units::set_units(st_area(geometry), ha)))

## area RDI
hhi <- 
  no_water %>% 
  mutate(total_area = sum(area),
         proportion = area / total_area,
         proportion = proportion ^ 2) %>%
  pull(proportion) %>%
  sum()

# 0.89
rdi <- 1 - hhi

## add population
population <- 
  reduce(
    map(codes %>% 
          str_remove_all("\'") %>% 
          str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          tidycensus::get_acs(state = str_sub(x, 1, 2), county = str_sub(x, 3, 5), 
                              variables = "B00001_001", geography = 'block group')
        }),
    rbind)

population <- 
  shape %>% 
  left_join(population) %>% 
  transmute(GEOID,
            population = estimate) %>%
  st_as_sf()

plot(population)
plot(aggregates)

population[is.na(population)] <- 0

aggregates <-
  no_water %>% 
  rownames_to_column(var = 'ID')

by_population <-
  areal::aw_interpolate(aggregates,
                        tid = "ID",
                        source = st_transform(population, 3857),
                        sid = "GEOID",
                        weight = 'sum',
                        extensive = "population")

## population RDI
hhi <- 
  by_population %>% 
  mutate(total_population = sum(population),
         proportion = population / total_population,
         proportion = proportion ^ 2) %>%
  pull(proportion) %>%
  sum()

# 0.94
rdi <- 1 - hhi

## explaining the concept
ggplot(by_population, aes(fill = population)) +
  geom_sf(size = 0, colour = '#ffffff') +
  geom_sf(data = counties, 
          aes(), colour = '#ffffff', alpha = 0, size = 0, linetype = 2, fill = NA) +
  scale_fill_gradientn(colours = scico::scico(palette = 'hawaii', n = 9),
                       limits = c(0, 4*10^4),
                       breaks = c(10000, 20000, 30000),
                       oob = scales::squish,
                       guide = guide_continuous,
                       name = 'population') +
  labs(title = "Railroad Division Index",
       subtitle = 'Railroads, Motorways, Rivers') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold', size = 10, colour = '#7c7c7c', hjust = 0.5),
        plot.subtitle = element_text(size = 20, colour = '#000000', hjust = 0.5),
        legend.title = element_text(face = 'bold'))

ggsave("RDI_1.png", height = 10, width = 10, dpi = 300)

ggplot(by_population, aes(fill = population)) +
  geom_sf(size = 0, colour = '#ffffff') +
  geom_sf(data = counties, 
          aes(), colour = '#7c7c7c', alpha = 0.5, size = 0.5, linetype = 2, fill = NA) +
  scale_fill_gradientn(colours = scico::scico(palette = 'hawaii', n = 9),
                       limits = c(0, 4*10^4),
                       breaks = c(10000, 20000, 30000),
                       oob = scales::squish,
                       guide = guide_continuous,
                       name = 'population') +
  labs(title = "Railroad Division Index",
       subtitle = 'Railroads, Motorways, Rivers') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold', size = 10, colour = '#7c7c7c', hjust = 0.5),
        plot.subtitle = element_text(size = 20, colour = '#000000', hjust = 0.5),
        legend.title = element_text(face = 'bold'))

ggsave("RDI_2.png", height = 10, width = 10, dpi = 300)

fs::dir_ls("./", regexp = "RDI_[0-9].png", type = 'file') %>% 
  magick::image_read() %>% 
  magick::image_join() %>% 
  magick::image_animate(fps = 0.5) %>% 
  magick::image_write("RDI_detroit_population.gif")
