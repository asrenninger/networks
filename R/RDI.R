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
  mutate(area = units::drop_units(units::set_units(st_area(geometry), ha))) %>%


ggplot(no_water, aes(fill = area)) +
  geom_sf(size = 0, colour = '#ffffff') +
  geom_sf(data = counties, 
          aes(), colour = '#ffffff', alpha = 0, size = 0, linetype = 2, fill = NA) +
  scale_fill_gradientn(colours = scico::scico(palette = 'hawaii', n = 9),
                       limits = c(1000, 100000),
                       breaks = c(20000, 40000, 60000, 80000),
                       oob = scales::squish,
                       guide = guide_continuous,
                       name = 'area (ha)') +
  labs(title = "Railroad Division Index",
       subtitle = 'Railroads, Motorways, Rivers') + 
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold', size = 10, colour = '#7c7c7c', hjust = 0.5),
        plot.subtitle = element_text(size = 20, colour = '#000000', hjust = 0.5),
        legend.title = element_text(face = 'bold'))

ggsave("RDI_1.png", height = 10, width = 10, dpi = 300)

ggplot(no_water, aes(fill = area)) +
  geom_sf(size = 0, colour = '#ffffff') +
  geom_sf(data = counties, 
          aes(), colour = '#7c7c7c', alpha = 0.5, size = 0.5, linetype = 2, fill = NA) +
  scale_fill_gradientn(colours = scico::scico(palette = 'hawaii', n = 9),
                       limits = c(1000, 100000),
                       breaks = c(20000, 40000, 60000, 80000),
                       oob = scales::squish,
                       guide = guide_continuous,
                       name = 'area (ha)') +
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
  magick::image_write("RDI.gif")