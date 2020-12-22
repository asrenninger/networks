########################################
## Moran's I and Regression
########################################

source("R/package.R")
source("R/help.R")

##

cents <- read_csv("data/processed/measurements.csv") %>%
  mutate(GEOID = as.character(GEOID))

##

projected <- block_groups("PA", "Philadelphia", cb = TRUE, class = 'sf') %>% st_transform(2824)

##

autocorrelating <- 
  cents %>%
  filter(month == 1 | month == 2) %>%
  group_by(GEOID) %>%
  summarise(baseline = mean(ind)) %>%
  right_join(cents) %>%
  select(GEOID, month, baseline, ind, out) %>%
  mutate(change = (ind - baseline) / (baseline + 1)) %>%
  filter(month == 4) %>%
  left_join(projected) %>%
  st_as_sf() 

coords <- 
  autocorrelating %>%
  st_centroid() %>%
  st_coordinates()

##

library(spdep)

##

nearest <- knn2nb(knearneigh(coords, 5))
weights <- nb2listw(nearest, style = "W")

##

moranstest <- moran.test(autocorrelating$change, weights)
montecarlo <- moran.mc(autocorrelating$change, weights, nsim = 999)

ggplot(as.data.frame(montecarlo$res), aes(montecarlo$res)) + 
  geom_histogram(bins = 500) +
  geom_vline(aes(xintercept = montecarlo$statistic), colour = pal[1], size = 0.5, linetype = 2) +
  geom_text(aes(x = montecarlo$statistic - 0.05, y = 50, label = glue("observed: I={round(montecarlo$statistic, 3)} | P={montecarlo$p.value}")), angle = 90, fontface = 'bold', colour = pal[1]) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "permuted I-value", y = "count") +
  theme_ver() +
  ggsave("moranmc.png", height = 4, width = 6, dpi = 300)

##

moransi <- localmoran(autocorrelating$change, weights) %>% as_tibble()

##

autocorrelating <- 
  autocorrelating %>%
  bind_cols(moransi) %>%
  rename(locali = Ii,
         expectation = E.Ii,
         variance = Var.Ii,
         deviation = Z.Ii,
         p_value = `Pr(z > 0)`)

##

map_moran_difference <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(change, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = rev(pal[c(1, 3, 5, 7, 9)]),
                    labels = str_sub(as.character(quantile(autocorrelating$change,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 1, 4),
                    name = "difference",
                    guide = guide_discrete) +
  labs(title = "degree change") +
  theme_map() +
  theme(legend.positio = 'bottom')

map_moran_i <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(locali, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = rev(pal[c(1, 3, 5, 7, 9)]),
                    labels = str_sub(as.character(quantile(autocorrelating$locali,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 1, 4),
                    name = "i value",
                    guide = guide_discrete) +
  labs(title = "local moran's i") +
  theme_map() +
  theme(legend.positio = 'bottom')

map_moran_p <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(p_value, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = rev(pal[c(1, 3, 5, 7, 9)]),
                    labels = str_sub(as.character(quantile(autocorrelating$p_value,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 1, 4),
                    name = "p value",
                    guide = guide_discrete) +
  labs(title = "p value") +
  theme_map() +
  theme(legend.positio = 'bottom')

##

library(patchwork)

##

patched <- map_moran_difference + map_moran_i + map_moran_p

##

ggsave("moran.png", height = 10, width = 30, dpi = 300)

##

autocorrelating <- 
  autocorrelating %>%
  mutate(scaled_difference = scale(change)) %>%
  select(change, scaled_difference, locali, expectation, variance, deviation, p_value) %>%
  mutate(lagged_difference = lag.listw(weights, scaled_difference),
         quad_sig = NA)

autocorrelating <-
  autocorrelating %>%
  mutate(quad_sig = 
           case_when(scaled_difference >= 0 & lagged_difference >= 0 & p_value <= 0.05 ~ 1,
                     scaled_difference <= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 2,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 3,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 4,
                     scaled_difference <= 0 & lagged_difference >= 0 & p_value <= 0.05 ~ 5)) %>%
  st_as_sf()

##

map_quads <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(quad_sig)), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 2),
                    name = "quadrants",
                    labels = c("high-high", "low-low"),
                    guide = guide_discrete,
                    na.translate = FALSE) +
  labs(title = "local moran's i") +
  theme_map() +
  theme(legend.position = 'bottom')

##

library(gwrr)
library(spgwr)

##

amenities <- 
  phila %>%
  filter(str_detect(str_to_lower(top_category), "restaurant|bar")) %>%
  group_by(GEOID) %>%
  summarise(amenities = n()) %>%
  st_drop_geometry()

regression <- 
  cents %>%
  filter(month == 1 | month == 2) %>%
  group_by(GEOID) %>%
  summarise(baseline = mean(out)) %>%
  right_join(cents) %>%
  select(GEOID, month, baseline, out) %>%
  mutate(change = (out - baseline) / (baseline)) %>%
  left_join(demos) %>%
  filter(month != 1 & month != 2) %>%
  left_join(projected) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry),
         area = units::set_units(area, km^2)) %>%
  mutate(population_density = population / area) %>%
  drop_na() %>%
  left_join(amenities) %>%
  mutate(amenity_density = amenities / area) %>%
  replace_na(list(amenity_density = 0)) %>%
  filter(month == 4) %>%
  st_as_sf() %>%
  as('Spatial')

##

bandwidth <- gwr.sel(change ~ population_density,
                     data = regression)

geogress <- 
  gwr(change ~ population_density + amenity_density + log(median_income) + household_size + lack_healthcare + college_degree + nonwhite,
      data = regression, 
      bandwidth = bandwidth)

##

background <- 
  projected %>% 
  st_union() %>% 
  st_combine()

##

ggplot() + 
  geom_sf(data = background,
          aes(), fill = '#707070', colour = NA, size = 0) +
  geom_sf(data = 
            geogress$SDF %>%
            st_as_sf() %>%
            # clean_names() %>%
            select(localR2),
          aes(fill = factor(ntile(localR2, 9))), size = 0.01, colour = '#ffffff') +
  #geom_sf(data =
  #          regression %>% 
  #          st_as_sf() %>%
  #          mutate(coefficient = geogress$SDF$localR2) %>%
  #          select(coefficient) %>%
  #          filter(coefficient > 0.5) %>%
  #          st_union() %>%
  #          st_combine(), 
  #        aes(), fill = NA, colour = 'grey70') +
  scale_fill_manual(values = rev(pal),
                    labels = str_sub(as.character(quantile(geogress$SDF$localR2,
                                                           c(.1,.2,.3,.4,.5,.6,.7,.8,.9),
                                                           na.rm = TRUE)), 1, 4),
                    name = "r-squared value",
                    guide = guide_discrete) +
  #labs(title = "Local R-Squared") +
  theme_map()  +
  theme(legend.position = 'bottom') +
  ggsave("rsquared.png", height = 8, width = 6, dpi = 300)

##

blocks <- block_groups("PA", "Philadelphia", cb = TRUE, class = 'sf') 

##

centroid <- 
  phila %>% 
  filter(str_detect(location_name, "City Hall")) %>% 
  st_transform(2824) %>%
  st_coordinates() %>% 
  as_tibble()

location <- 
  blocks%>%
  st_transform(2824) %>%
  st_centroid() %>%
  st_coordinates() %>% 
  as_tibble()

##

library(spdep)
library(FNN)

##

nn <- get.knnx(centroid, location, k = 1)

distances <- 
  blocks %>% 
  transmute(GEOID = GEOID, distance = nn$nn.dist) %>%
  st_drop_geometry()

##

regression <- 
  cents %>%
  filter(month == 1 | month == 2) %>%
  group_by(GEOID) %>%
  summarise(baseline = mean(out)) %>%
  right_join(cents) %>%
  select(GEOID, month, baseline, out) %>%
  mutate(change = (out - baseline) / (baseline)) %>%
  left_join(demos) %>%
  left_join(distances) %>%
  filter(month != 1 & month != 2) %>%
  left_join(projected) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry),
         area = units::set_units(area, km^2)) %>%
  mutate(population_density = population / area) %>%
  drop_na() %>%
  left_join(amenities) %>%
  mutate(amenity_density = amenities / area) %>%
  replace_na(list(amenity_density = 0)) %>%
  filter(month == 4) %>%
  st_as_sf() %>%
  as('Spatial')

##

bandwidth <- gwr.sel(change ~ distance,
                     data = regression)

geogress <- 
  gwr(change ~ distance + population_density + amenity_density + log(median_income) + household_size + lack_healthcare + college_degree + nonwhite,
      data = regression, 
      bandwidth = bandwidth)

mean(geogress$SDF$localR2)
min(geogress$SDF$localR2)
max(geogress$SDF$localR2)

##

out_summary <- 
  geogress$SDF %>% 
  as_tibble() %>% 
  select(-sum.w, -gwr.e, -pred) %>%
  clean_names() %>%
  pivot_longer(names_to = "variable", x_intercept:local_r2) %>%
  group_by(variable) %>% 
  summarise(min = min(value),
            max = max(value),
            mean = mean(value),
            median = median(value)) %>%
  mutate_if(is.numeric, round(. , 5))
  mutate(variable = str_replace_all(variable, "_", " "),
         variable = str_replace_all(variable, "distance", "distance to city hall"),
         group = "out-degree") %>%
  filter(variable != "local r2") %>%
  mutate(sign = case_when(median > 0 ~ "+",
                          median < 0 ~ "-"))

##

ggplot() + 
  geom_sf(data = background,
          aes(), fill = '#707070', colour = NA, size = 0) +
  geom_sf(data = 
            geogress$SDF %>%
            st_as_sf() %>%
            # clean_names() %>%
            select(localR2),
          aes(fill = factor(ntile(localR2, 9))), size = 0.01, colour = '#ffffff') +
  #geom_sf(data =
  #          regression %>% 
  #          st_as_sf() %>%
  #          mutate(coefficient = geogress$SDF$localR2) %>%
  #          select(coefficient) %>%
  #          filter(coefficient > 0.5) %>%
  #          st_union() %>%
  #          st_combine(), 
  #        aes(), fill = NA, colour = 'grey70') +
  scale_fill_manual(values = rev(pal),
                    labels = str_sub(as.character(quantile(geogress$SDF$localR2,
                                                           c(.1,.2,.3,.4,.5,.6,.7,.8,.9),
                                                           na.rm = TRUE)), 1, 4),
                    name = "r-squared value",
                    guide = guide_discrete) +
  #labs(title = "Local R-Squared") +
  theme_map()  +
  theme(legend.position = 'bottom') +
  ggsave("rsquared.png", height = 8, width = 6, dpi = 300)

##

regression <- 
  cents %>%
  filter(month == 1 | month == 2) %>%
  group_by(GEOID) %>%
  summarise(baseline = mean(out)) %>%
  right_join(cents) %>%
  select(GEOID, month, baseline, ind) %>%
  mutate(change = (ind - baseline) / (baseline + 1)) %>%
  left_join(demos) %>%
  left_join(distances) %>%
  filter(month != 1 & month != 2) %>%
  left_join(projected) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry),
         area = units::set_units(area, km^2)) %>%
  mutate(population_density = population / area) %>%
  drop_na() %>%
  left_join(amenities) %>%
  mutate(amenity_density = amenities / area) %>%
  replace_na(list(amenity_density = 0)) %>%
  filter(month == 4) %>%
  st_as_sf() %>%
  as('Spatial')

##

bandwidth <- gwr.sel(change ~ distance,
                     data = regression)

geogress <- 
  gwr(change ~ distance + population_density + amenity_density + log(median_income) + household_size + lack_healthcare + college_degree + nonwhite,
      data = regression, 
      bandwidth = bandwidth)

lm(change ~ distance + population_density + amenity_density + log(median_income) + household_size + lack_healthcare + college_degree + nonwhite,
    data = regression %>% st_as_sf() %>% st_drop_geometry()) %>% summary()

##

mean(geogress$SDF$localR2)
min(geogress$SDF$localR2)
max(geogress$SDF$localR2)

##

in_summary <- 
  geogress$SDF %>% 
  as_tibble() %>% 
  select(-sum.w, -gwr.e, -pred) %>%
  clean_names() %>%
  pivot_longer(names_to = "variable", x_intercept:local_r2) %>%
  group_by(variable) %>% 
  summarise(min = min(value),
            max = max(value),
            mean = mean(value),
            median = median(value)) %>%
  mutate(variable = str_replace_all(variable, "_", " "),
         variable = str_replace_all(variable, "distance", "distance to city hall"),
         group = "in-degree") %>%
  filter(variable != "local r2") %>%
  mutate(sign = case_when(median > 0 ~ "+",
                          median < 0 ~ "-"))

##

library(gt)

##

gt(data = bind_rows(in_summary, out_summary), rowname_col = "variable", groupname_col = "group") %>%
  data_color(
    columns = vars(`sign`),
    colors = scales::col_factor(
      palette = pal[c(1, 9)],
      domain = NULL
    )) %>%
  tab_options(heading.title.font.size = 30,
              heading.subtitle.font.size = 15,
              heading.align = "left",
              table.border.top.color = "white",
              heading.border.bottom.color = "white",
              table.border.bottom.color = "white",
              column_labels.border.bottom.color = "grey",
              column_labels.border.bottom.width= px(1)) %>% 
  gtsave("regressions.png", expand = 10)

##

regression <- 
  cents %>%
  filter(month == 1 | month == 2) %>%
  group_by(GEOID) %>%
  summarise(baseline = mean(prc)) %>%
  right_join(cents) %>%
  select(GEOID, month, baseline, prc) %>%
  mutate(change = (prc - baseline)) %>%
  left_join(demos) %>%
  left_join(distances) %>%
  filter(month != 1 & month != 2) %>%
  left_join(projected) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry),
         area = units::set_units(area, km^2)) %>%
  mutate(population_density = population / area) %>%
  drop_na() %>%
  left_join(amenities) %>%
  mutate(amenity_density = amenities / area) %>%
  replace_na(list(amenity_density = 0)) %>%
  filter(month == 4) %>%
  st_as_sf() %>%
  as('Spatial')

##

bandwidth <- gwr.sel(change ~ distance,
                     data = regression)

geogress <- 
  gwr(change ~ distance + population_density + amenity_density + log(median_income) + household_size + lack_healthcare + college_degree + nonwhite,
      data = regression, 
      bandwidth = bandwidth)

##

mean(geogress$SDF$localR2)
min(geogress$SDF$localR2)
max(geogress$SDF$localR2)

mean(geogress$SDF$log.median_income.)
min(geogress$SDF$log.median_income.)
max(geogress$SDF$log.median_income.)

##

geogress$SDF %>% 
  as_tibble() %>% 
  select(-sum.w, -gwr.e, -pred) %>%
  clean_names() %>%
  pivot_longer(names_to = "variable", x_intercept:local_r2) %>%
  group_by(variable) %>% 
  summarise(min = min(value),
            max = max(value),
            mean = mean(value),
            median = median(value)) %>%
  mutate(variable = str_replace_all(variable, "_", " "),
         variable = str_replace_all(variable, "distance", "distance to city hall"),
         group = "in-degree") %>%
  filter(variable != "local r2") %>% 
  as.data.frame()
