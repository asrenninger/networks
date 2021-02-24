####################################
## Philadelphia, simplex
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")

## getting fips codes, metro or county
codes <- get_codes("philadelphia")
codes <- "'42101'"

## getting node data
nodes <- get_nodes(codes)

## getting edge data 
index <- str_pad(1:12, side = 'left', width = 2, pad = "0")
edges <- map_df(index, function(x) { get_edges(codes, x, nodes$cbg) %>% mutate(month = as.numeric(x)) })

## getting context data
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

## creating background
water <- 
  water %>% 
  st_union() %>%
  st_combine()

background <-
  shape %>% 
  st_union() %>% 
  st_combine() %>% 
  st_difference(water) %>% 
  rmapshaper::ms_simplify(0.05)

##  plotting desire lines
lines <- stplanr::od2line(edges, 
                          nodes %>% 
                            st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
                            st_transform(4269))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = lines %>%
            mutate(month = lubridate::month(month, label = TRUE, abbr = FALSE)), 
          aes(colour = factor(ntile(weight, 7)), lwd = weight, alpha = weight)) +
  scale_colour_manual(values = scico::scico(7, palette = 'tokyo'),
                      labels = as.character(quantile(lines$weight,
                                                     c(.2,.3,.4,.5,.6,.7,.8),
                                                     na.rm = TRUE)),
                      name = "visits",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ month, ncol = 4) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "flows.png", height = 6, width = 8, dpi = 300)

## getting distances
distances <- 
  lines %>% 
  mutate(distance = units::drop_units(st_length(geometry))) %>% 
  st_drop_geometry() 

## getting demography
income <- 
  vroom("data/census/data/cbg_b19.csv") %>% 
  filter(census_block_group %in% shape$GEOID) %>%
  select(census_block_group, B19301e1, B19301m1) %>% 
  transmute(target = census_block_group,
            median_income = B19301e1)

education <- 
  vroom("data/census/data/cbg_b15.csv") %>% 
  filter(census_block_group %in% shape$GEOID) %>%
  select(census_block_group, B15003e1, B15003e22, B15003m1, B15003m22) %>%
  transmute(target = census_block_group, 
            college_degree = B15003e22 / B15003e1)

size <- 
  vroom("data/census/data/cbg_b25.csv") %>% 
  filter(census_block_group %in% shape$GEOID) %>%
  select(census_block_group, B25010e1) %>%
  transmute(target = census_block_group, 
            household_size = B25010e1)

population <- 
  vroom("data/census/data/cbg_b01.csv") %>% 
  filter(census_block_group %in% shape$GEOID) %>%
  select(census_block_group, B01001e1) %>%
  transmute(target = census_block_group, 
            population = B01001e1)

## poi data for the model
pois <- get_pois(codes, "01")

businesses <- 
  pois %>% 
  group_by(poi_cbg) %>%
  summarise(businesses = n()) %>%
  rename(focal = poi_cbg)

## first constraint
D_j <- distances %>% group_by(focal) %>% summarise(D_j = sum(weight))
O_i <- distances %>% group_by(target) %>% summarise(O_i = sum(weight))

## join it all together
regression <- 
  distances %>%
  filter(focal != target) %>% 
  left_join(population) %>%
  left_join(education) %>%
  left_join(income) %>%
  left_join(size) %>%
  left_join(O_i) %>%
  left_join(D_j) %>%
  left_join(businesses) %>%
  replace_na(list(businesses = 0)) %>%
  as_tibble() %>% 
  drop_na() %>%
  select(-focal, -target)

## testing on a month
temp <- filter(regression, month == 1)

gravity <- 
  glm(log(weight) ~
        log(distance) +  population +
        log(median_income) +  log(businesses + 1) + college_degree + household_size + 
        + D_j + O_i, family = poisson(link = "log"), 
      data = temp)

summary(gravity)

gravity %>% 
  broom::tidy() %>% 
  mutate(estimate = case_when(str_detect(term, "log") ~ exp(estimate),
                                                          TRUE ~ estimate)) %>% 
  mutate_if(is.numeric, ~round(.x, 4)) %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(vars(`term`))) %>% 
  data_color(columns = vars(`estimate`),
             colors = scales::col_numeric(scico::scico(9, palette = 'tokyo'), domain = NULL)) %>% 
  gtsave("mod2.png", expand = 10)

length(nodes$cbg) *  length(nodes$cbg) 

## every month
fits <- 
  reduce(map(1:12, function(x){
    
    temp <- filter(regression, month == x)
    
    gravity <- 
      glm(log(weight) ~
            log(distance) + 
            population + college_degree + household_size + 
            log(median_income) +  log(businesses + 1) + 
            log(D_j) + log(O_i), family = poisson(link = "log"), data = temp)
    
    temp$predictions <- exp(fitted(gravity))
    
    return(rsquared(temp$weight, temp$predictions))
  }), 
  c
  )

## how does the fit change over time?
ggplot(data = tibble(period = 1:12, 
                     fit = fits), 
       aes(x = period, y = fit)) + 
  geom_step(size = 2, colour = scico::scico(palette = 'tokyo', 9)[8]) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = lubridate::month(c(2, 4, 6, 8, 10), label = TRUE)) +
  xlab("") +
  ylab("") + 
  labs(title = "Variance Explained Over Time") +
  theme_hor() + 
  ggsave("rsquared_phl.png", height = 6, width = 8, dpi = 300)

## try again with rmse
fits <- 
  reduce(map(1:12, function(x){
    
    temp <- filter(regression, month == x)
    
    gravity <- 
      glm(log(weight) ~
            log(distance) + 
            population + college_degree + household_size + 
            log(median_income) +  log(businesses + 1) + 
            log(D_j) + log(O_i), family = poisson(link = "log"), data = temp)
    
    temp$predictions <- exp(fitted(gravity))
    
    return(rmse(temp$weight, temp$predictions))
  }), 
  c
  )

## how does the fit change over time?
ggplot(data = tibble(period = 1:12, 
                     fit = fits), 
       aes(x = period, y = fit)) + 
  geom_step(size = 2, colour = scico::scico(palette = 'tokyo', 9)[8]) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = lubridate::month(c(2, 4, 6, 8, 10), label = TRUE)) +
  xlab("") +
  ylab("") + 
  labs(title = "RMSE Over Time") +
  theme_hor() + 
  ggsave("rmse_phl.png", height = 6, width = 8, dpi = 300)

## what about coefficients?
results <- 
  reduce(map(1:12, function(x){
    
    temp <- filter(regression, month == x)
    
    gravity <- 
      glm(log(weight) ~
            log(distance) + 
            population + college_degree + household_size + 
            log(median_income) +  log(businesses + 1) + 
            log(D_j) + log(O_i), family = poisson(link = "log"), data = temp)
    
    results <-  
      broom::tidy(gravity) %>% 
      mutate(month = x)
    
    return(results)
  }), 
  rbind
  )

## plot it
ggplot(data = results %>%
         filter(term != "(Intercept)") %>%
         mutate(term = str_replace_all(term, "_", " ")), 
       aes(x = month, y = estimate, colour = term)) + 
  geom_step(size = 2) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = lubridate::month(c(2, 4, 6, 8, 10), label = TRUE)) +
  scale_colour_manual(values = sample(scico(palette = 'tokyo', 9)[1:8], 8)) +
  facet_wrap(~ term, scales = 'free_y', nrow = 2) + 
  xlab("") +
  ylab("") + 
  labs(title = "Coefficients Over Time") +
  theme_hor() +
  theme(legend.position = 'botoom') +
  ggsave("coefficients_phl.png", height = 6, width = 8, dpi = 300)

fits <- 
  reduce(map(1:12, function(x){
    
    temp <- filter(regression, month == x)
    
    gravity <- 
      glm(log(weight) ~
            log(distance) + 
            population + college_degree + household_size + 
            log(median_income) +  log(businesses + 1) + 
            log(D_j) + log(O_i), family = poisson(link = "log"), data = temp)
    
    temp$predictions <- exp(fitted(gravity))
    
    return(mape(temp$weight, temp$predictions))
  }), 
  c
  )

## how does the fit change over time?
ggplot(data = tibble(period = 1:12, 
                     fit = fits), 
       aes(x = period, y = fit)) + 
  geom_step(size = 2, colour = scico::scico(palette = 'tokyo', 9)[8]) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), labels = lubridate::month(c(2, 4, 6, 8, 10), label = TRUE)) +
  xlab("") +
  ylab("") + 
  labs(title = "Mean Absolute Percent Error Over Time") +
  theme_hor() + 
  ggsave("mape_phl.png", height = 6, width = 8, dpi = 300)

####################################
## Philadelphia, multiplex
####################################

## get pois
pois <- get_pois(codes, "01")

## get nodes
nodes <- get_nodes("'42101'")

## get edges
index <- str_pad(1:12, side = 'left', width = 2, pad = "0")
edges <- map_df(index, function(x) { get_bipartite(codes, x, nodes$cbg, 0) %>% mutate(month = as.numeric(x)) })

edges %>%
  filter(str_detect(top_category, "Grocery"))

## get block groups
shape <- block_groups('PA', "Philadelphia", cb = TRUE, class = 'sf')
tract <-  
  shape %>% 
  mutate(GEOID = str_sub(GEOID, 1, 11)) %>% 
  group_by(GEOID) %>%
  summarise() %>% 
  st_transform(4326)

## attached origin centroid
cent_o <-
  edges %>% 
  #filter(str_detect(sub_category, "Grocery")) %>%
  #filter(visits > 20) %>%
  mutate(poi_cbg = str_sub(poi_cbg, 1, 11),
         home_cbg = str_sub(home_cbg, 1, 11)) %>%
  filter(poi_cbg != home_cbg) %>%
  group_by(month, home_cbg) %>%
  summarise(visits = sum(visits)) %>%
  rename(GEOID = home_cbg) %>%
  left_join(tract) %>%
  st_as_sf()

cent_o <-
  cent_o %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>%
  bind_cols(cent_o) %>%
  transmute(GEOID, 
            month, 
            latitude = Y,
            longitude = X,
            visits)

## create mean destination  
mean_d <- 
  edges %>%
  #filter(str_detect(sub_category, "Grocery")) %>%
  #filter(visits > 20) %>%
  mutate(poi_cbg = str_sub(poi_cbg, 1, 11),
         home_cbg = str_sub(home_cbg, 1, 11)) %>%
  filter(poi_cbg != home_cbg) %>%
  group_by(home_cbg, month) %>% 
  summarise(latitude = weighted.mean(latitude, w = visits, na.rm = TRUE),
            longitude = weighted.mean(longitude, w = visits, na.rm = TRUE),
            visits = sum(visits)) %>%
  rename(GEOID = home_cbg)

## plot interaction winds
ggplot() +
  geom_path(data = bind_rows(cent_o, mean_d), 
            aes(x = longitude, y = latitude, group = GEOID, alpha = log(visits)), colour = '#ffffff', arrow = grid::arrow(length = unit(0.05, unit = "inches"), type = 'closed')) +
  geom_sf(data = tract %>%
            st_union() %>%
            st_combine(), 
          aes(), fill = NA, colour = '#ffffff', lwd = 0.05, alpha = 0.01) + 
  #scale_colour_scico(palette = 'tokyo', guide = 'none') +
  scale_alpha_continuous(range = c(0.25, 0.50), guide = 'none') +
  facet_wrap(~ lubridate::month(month, label = TRUE, abbr = FALSE), ncol = 4) + 
  coord_sf(crs = 4326) + 
  #labs(title = "Interaction Winds") +
  theme_black() +
  ggsave("winds_grocery.png", height = 15.1, width = 18, dpi = 300)

####################################
## Thresholding
####################################

## start with flow graphs 
library(ggraph)

graph_df <- map_df(c(25, 50, 75, 100), function(x){
  graph_df <-
    edges %>% 
    filter(poi_cbg != home_cbg) %>%
    group_by(home_cbg, poi_cbg, month) %>%
    summarise(weight = sum(visits)) %>%
    filter(weight > x) %>% 
    rename(from = home_cbg,
           to = poi_cbg) %>%
    mutate(threshold = x)
})

graph <- graph_from_data_frame(graph_df)

ggraph(graph, 'circle') + 
  geom_edge_link(aes(alpha = log(weight)), colour = '#ffffff', show.legend = FALSE) + 
  geom_node_point(colour = '#ffffff', size = 0.1) +
  scale_alpha_continuous(range = c(0.25, 0.75), guide = 'none') + 
  facet_grid(threshold ~ lubridate::month(month, label= TRUE, abbr = FALSE)) + 
  coord_fixed() +
  theme_black() + 
  ggsave("thresholds_monthxthreshold.png", height = 5, width = 13.8, dpi = 300)

graph_df <-
  edges %>% 
  filter(poi_cbg != home_cbg) %>%
  group_by(home_cbg, poi_cbg, month) %>%
  summarise(weight = sum(visits, na.rm = TRUE)) %>%
  filter(weight > 50) %>% 
  rename(from = home_cbg,
         to = poi_cbg)

graph <- graph_from_data_frame(graph_df)

ggraph(graph, 'linear', circular = TRUE) + 
  geom_edge_link(aes(alpha = log(weight)), colour = '#ffffff', show.legend = FALSE) + 
  geom_node_point(colour = '#ffffff', size = 0.1) +
  scale_alpha_continuous(range = c(0.25, 0.75), guide = 'none') + 
  facet_wrap(~ lubridate::month(month, label= TRUE, abbr = FALSE)) + 
  coord_fixed() +
  theme_black() + 
  ggsave("thresholds_month.png", height = 6, width = 7.4, dpi = 300)

####################################
## modelling
####################################

edges_grocery <- 
  edges %>% 
  filter(str_detect(sub_category, "Grocery")) %>% 
  group_by(poi_cbg, home_cbg) %>% 
  summarise(weight = sum(visits)) %>% 
  rename(focal = poi_cbg,
        target = home_cbg) 

lines <- stplanr::od2line(edges_grocery, 
                          nodes %>% 
                            st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
                            st_transform(4269))

footprints <- 
  vroom("data/footprints/may2020release/SafeGraphPlacesGeoSupplementSquareFeet.csv.gz") %>% 
  filter(safegraph_place_id %in% edges$poi_id)

businesses <- 
  pois %>% 
  left_join(footprints) %>%
  group_by(poi_cbg) %>%
  summarise(businesses = n(), 
            floor_area = sum(area_square_feet, na.rm = TRUE)) %>%
  rename(focal = poi_cbg)

grocers <- 
  pois %>% 
  filter(str_detect(sub_category, "Grocery")) %>%
  left_join(footprints) %>%
  group_by(poi_cbg) %>%
  summarise(grocers = n(), 
            grocers_area = sum(area_square_feet, na.rm = TRUE)) %>%
  rename(focal = poi_cbg)

distances <- 
  lines %>% 
  mutate(distance = units::drop_units(st_length(geometry))) %>% 
  st_drop_geometry() 

D_j <- distances %>% group_by(focal) %>% summarise(D_j = sum(weight))
O_i <- distances %>% group_by(target) %>% summarise(O_i = sum(weight))

centroid <- 
  pois %>%
  filter(str_detect(location_name, "City Hall")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(4269) %>% 
  st_centroid() %>%
  st_coordinates() %>% 
  as_tibble()

location <- 
  shape %>%
  st_centroid() %>%
  st_coordinates() %>% 
  as_tibble()

nn <- FNN::get.knnx(centroid, location, k = 1)

centrality <- 
  shape %>% 
  st_drop_geometry() %>% 
  transmute(focal = GEOID, 
            centrality = nn$nn.dist[, 1])

## join it all together
regression <- 
  distances %>%
  filter(focal != target) %>% 
  left_join(population) %>%
  left_join(education) %>%
  left_join(income) %>%
  left_join(size) %>%
  left_join(O_i) %>%
  left_join(D_j) %>%
  left_join(businesses) %>%
  left_join(grocers) %>%
  left_join(centrality) %>%
  replace_na(list(businesses = 0, grocers = 0)) %>%
  as_tibble() %>% 
  drop_na() %>%
  select(-focal, -target)

gravity <- 
  glm(log(weight) ~
        log(distance) + 
        population + 
        #log(grocers + 1) + 
        log(grocers_area + 1), 
      family = poisson(link = "log"), 
      data = regression)

summary(gravity)

regression$lean <- exp(fitted(gravity))
hist(abs(regression$weight - regression$lean), breaks = 50)

ggplot(regression %>% 
         filter(weight < 200), aes(x = weight, y = lean)) +
  geom_hex(bins = 100) + 
  geom_abline(colour = '#848484', size = 1, linetype = 2) +
  scale_fill_gradientn(colours = pal, guide = guide_continuous) +
  theme_ver() +
  labs(title =  glue("MAE: {round(mae(regression$weight, regression$lean), 4)} | SDAE: {round(sdae(regression$weight, regression$lean), 4)} | MAPE: {round(mape(regression$weight, regression$lean), 4)} | SDAPE: {round(sdape(regression$weight, regression$lean), 4)}"),
       caption = glue("mean observed and mean predicted, {round(mean(regression$weight), 2)} and {round(mean(regression$lean), 2)}")) +
  theme(legend.position = 'bottom') +
  ggsave("observedxpredicted_1.png", height = 8, width = 8, dpi = 300)

error_lines <- 
  regression %>% 
  left_join(distances) %>% 
  left_join(lines) %>% 
  st_as_sf() %>% 
  mutate(percent_error = abs(weight - lean) / weight,
         absolute_error = abs(weight - lean),
         error = weight - lean,
         quartile = ntile(error, 4))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = error_lines, 
          aes(colour = factor(ntile(error, 9)), lwd = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_lines$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ quartile, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "absolute_flows_1.png", height = 6, width = 8, dpi = 300)

error_blocks <-
  error_lines %>%
  st_drop_geometry() %>% 
  group_by(focal) %>% 
  summarise(percent_error = mean(percent_error),
            absolute_error = mean(absolute_error),
            error = mean(error)) %>%
  mutate(quartile_error = ntile(error, 4),
         quartile_absolute = ntile(absolute_error, 4), 
         GEOID = focal) %>%
  left_join(shape) %>%
  st_as_sf()

fills <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
          aes(X, Y, colour = factor(ntile(absolute_error, 9)), size = absolute_error, alpha = absolute_error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$absolute_error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_absolute, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills, filename = "absolute_blocks_1.png", height = 6, width = 8, dpi = 300)

fills_error <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(error, 9)), size = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_error, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills_error, filename = "error_blocks_1.png", height = 6, width = 8, dpi = 300)

## new model (skip this one)
gravity <- 
  glm(log(weight) ~
        log(distance) + 
        population + 
        #log(businesses + 1) + 
        log(floor_area + 1), 
      family = poisson(link = "log"), 
      data = regression)

summary(gravity)

regression$predictions <- exp(fitted(gravity))
hist(abs(regression$weight - regression$predictions), breaks = 50)

ggplot(regression %>% 
         filter(weight < 200), aes(x = weight, y = predictions)) +
  geom_hex(bins = 100) + 
  geom_abline(colour = '#848484', size = 1, linetype = 2) +
  scale_fill_gradientn(colours = pal, guide = guide_continuous) +
  theme_ver() +
  labs(title =  glue("MAE: {round(mae(regression$weight, regression$predictions), 4)} | SDAE: {round(sdae(regression$weight, regression$predictions), 4)} | MAPE: {round(mape(regression$weight, regression$predictions), 4)} | SDAPE: {round(sdape(regression$weight, regression$predictions), 4)}"),
       caption = glue("mean observed and mean predicted, {round(mean(regression$weight), 2)} and {round(mean(regression$predictions), 2)}")) +
  theme(legend.position = 'bottom') +
  ggsave("observedxpredicted_2.png", height = 8, width = 8, dpi = 300)

error_lines <- 
  regression %>% 
  left_join(distances) %>% 
  left_join(lines) %>% 
  st_as_sf() %>% 
  mutate(percent_error = abs(weight - predictions) / weight,
         absolute_error = abs(weight - predictions),
         error = weight - predictions,
         quartile = ntile(error, 4))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = error_lines, 
          aes(colour = factor(ntile(error, 9)), lwd = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_lines$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ quartile, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "absolute_flows_2.png", height = 6, width = 8, dpi = 300)

error_blocks <-
  error_lines %>%
  st_drop_geometry() %>% 
  group_by(focal) %>% 
  summarise(percent_error = mean(percent_error),
            absolute_error = mean(absolute_error),
            error = mean(error)) %>%
  mutate(quartile_error = ntile(error, 4),
         quartile_absolute = ntile(absolute_error, 4), 
         GEOID = focal) %>%
  left_join(shape) %>%
  st_as_sf()

fills <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(absolute_error, 9)), size = absolute_error, alpha = absolute_error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$absolute_error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_absolute, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills, filename = "absolute_blocks_2.png", height = 6, width = 8, dpi = 300)

fills_error <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(error, 9)), size = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_error, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills_error, filename = "error_blocks_2.png", height = 6, width = 8, dpi = 300)

## penultimate model
gravity <- 
  glm(log(weight) ~
        log(distance) + 
        population + 
        #log(grocers + 1) + 
        log(grocers_area + 1) +
        log(O_i), 
      family = poisson(link = "log"), 
      data = regression)

summary(gravity)

regression$singly <- exp(fitted(gravity))
hist(abs(regression$weight - regression$singly), breaks = 50)

ggplot(regression %>% 
         filter(weight < 200), aes(x = weight, y = singly)) +
  geom_hex(bins = 100) + 
  geom_abline(colour = '#848484', size = 1, linetype = 2) +
  scale_fill_gradientn(colours = pal, guide = guide_continuous) +
  theme_ver() +
  labs(title =  glue("MAE: {round(mae(regression$weight, regression$singly), 4)} | SDAE: {round(sdae(regression$weight, regression$singly), 4)} | MAPE: {round(mape(regression$weight, regression$singly), 4)} | SDAPE: {round(sdape(regression$weight, regression$singly), 4)}"),
       caption = glue("mean observed and mean predicted, {round(mean(regression$weight), 2)} and {round(mean(regression$singly), 2)}")) +
  theme(legend.position = 'bottom') +
  ggsave("observedxpredicted_3.png", height = 8, width = 8, dpi = 300)

error_lines <- 
  regression %>% 
  left_join(distances) %>% 
  left_join(lines) %>% 
  st_as_sf() %>% 
  mutate(percent_error = abs(weight - singly) / weight,
         absolute_error = abs(weight - singly),
         error = weight - singly,
         quartile = ntile(error, 4))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = error_lines, 
          aes(colour = factor(ntile(error, 9)), lwd = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_lines$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ quartile, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "absolute_flows_3.png", height = 6, width = 8, dpi = 300)

error_blocks <-
  error_lines %>%
  st_drop_geometry() %>% 
  group_by(focal) %>% 
  summarise(percent_error = mean(percent_error),
            absolute_error = mean(absolute_error),
            error = mean(error)) %>%
  mutate(quartile_error = ntile(error, 4),
         quartile_absolute = ntile(absolute_error, 4), 
         GEOID = focal) %>%
  left_join(shape) %>%
  st_as_sf()

fills <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(absolute_error, 9)), size = absolute_error, alpha = absolute_error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$absolute_error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_absolute, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills, filename = "absolute_blocks_3.png", height = 6, width = 8, dpi = 300)

fills_error <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(error, 9)), size = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_error, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills_error, filename = "error_blocks_3.png", height = 6, width = 8, dpi = 300)

## final model
gravity <- 
  glm(log(weight) ~
        log(distance) + 
        population + 
        #log(grocers + 1) + 
        log(grocers_area + 1) +
        log(O_i) +
        log(D_j), 
      family = poisson(link = "log"), 
      data = regression)

summary(gravity)

regression$doubly <- exp(fitted(gravity))
hist(abs(regression$weight - regression$doubly), breaks = 50)

ggplot(regression %>% 
         filter(weight < 200), aes(x = weight, y = doubly)) +
  geom_hex(bins = 100) + 
  geom_abline(colour = '#848484', size = 1, linetype = 2) +
  scale_fill_gradientn(colours = pal, guide = guide_continuous) +
  theme_ver() +
  labs(title =  glue("MAE: {round(mae(regression$weight, regression$doubly), 4)} | SDAE: {round(sdae(regression$weight, regression$doubly), 4)} | MAPE: {round(mape(regression$weight, regression$doubly), 4)} | SDAPE: {round(sdape(regression$weight, regression$doubly), 4)}"),
       caption = glue("mean observed and mean predicted, {round(mean(regression$weight), 2)} and {round(mean(regression$doubly), 2)}")) +
  theme(legend.position = 'bottom') +
  ggsave("observedxpredicted_4.png", height = 8, width = 8, dpi = 300)

error_lines <- 
  regression %>% 
  left_join(distances) %>% 
  left_join(lines) %>% 
  st_as_sf() %>% 
  mutate(percent_error = abs(weight - doubly) / weight,
         absolute_error = abs(weight - doubly),
         error = weight - doubly,
         quartile = ntile(error, 4))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = error_lines, 
          aes(colour = factor(ntile(error, 9)), lwd = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_lines$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ quartile, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "absolute_flows_4.png", height = 6, width = 8, dpi = 300)

error_blocks <-
  error_lines %>%
  st_drop_geometry() %>% 
  group_by(focal) %>% 
  summarise(percent_error = mean(percent_error),
            absolute_error = mean(absolute_error),
            error = mean(error)) %>%
  mutate(quartile_error = ntile(error, 4),
         quartile_absolute = ntile(absolute_error, 4), 
         GEOID = focal) %>%
  left_join(shape) %>%
  st_as_sf()

fills <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(absolute_error, 9)), size = absolute_error, alpha = absolute_error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$absolute_error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_absolute, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills, filename = "absolute_blocks_4.png", height = 6, width = 8, dpi = 300)

fills_error <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(error, 9)), size = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_error, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills_error, filename = "error_blocks_4.png", height = 6, width = 8, dpi = 300)

## kitchen sink model
gravity <- 
  glm(log(weight) ~
        log(distance) + 
        population + 
        log(grocers + 1) + 
        log(grocers_area + 1) +
        log(median_income) +
        log(O_i) +
        log(D_j), 
      family = poisson(link = "log"), 
      data = regression)

summary(gravity)

regression$predictions <- exp(fitted(gravity))
hist(abs(regression$weight - regression$predictions), breaks = 50)

ggplot(regression %>% 
         filter(weight < 200), aes(x = weight, y = predictions)) +
  geom_hex(bins = 100) + 
  geom_abline(colour = '#848484', size = 1, linetype = 2) +
  scale_fill_gradientn(colours = pal, guide = guide_continuous) +
  theme_ver() +
  labs(title =  glue("MAE: {round(mae(regression$weight, regression$predictions), 4)} | SDAE: {round(sdae(regression$weight, regression$predictions), 4)} | MAPE: {round(mape(regression$weight, regression$predictions), 4)} | SDAPE: {round(sdape(regression$weight, regression$predictions), 4)}"),
       caption = glue("mean observed and mean predicted, {round(mean(regression$weight), 2)} and {round(mean(regression$predictions), 2)}")) +
  theme(legend.position = 'bottom') +
  ggsave("observedxpredicted_5.png", height = 8, width = 8, dpi = 300)

error_lines <- 
  regression %>% 
  left_join(distances) %>% 
  left_join(lines) %>% 
  st_as_sf() %>% 
  mutate(percent_error = abs(weight - predictions) / weight,
         absolute_error = abs(weight - predictions),
         error = weight - predictions,
         quartile = ntile(error, 4))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = error_lines, 
          aes(colour = factor(ntile(error, 9)), lwd = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_lines$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ quartile, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "absolute_flows_5.png", height = 6, width = 8, dpi = 300)

error_blocks <-
  error_lines %>%
  st_drop_geometry() %>% 
  group_by(focal) %>% 
  summarise(percent_error = mean(percent_error),
            absolute_error = mean(absolute_error),
            error = mean(error)) %>%
  mutate(quartile_error = ntile(error, 4),
         quartile_absolute = ntile(absolute_error, 4), 
         GEOID = focal) %>%
  left_join(shape) %>%
  st_as_sf()

fills <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(absolute_error, 9)), size = absolute_error, alpha = absolute_error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$absolute_error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_absolute, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills, filename = "absolute_blocks_5.png", height = 6, width = 8, dpi = 300)

fills_error <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(error, 9)), size = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_error, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills_error, filename = "error_blocks_5.png", height = 6, width = 8, dpi = 300)

## once more, with distance to city hall
gravity <- 
  glm(log(weight) ~
        log(distance) + 
        population + 
        #log(grocers + 1) + 
        log(grocers_area + 1) +
        log(median_income) +
        centrality + 
        log(O_i) +
        log(D_j), 
      family = poisson(link = "log"), 
      data = regression)

summary(gravity)

regression$predictions <- exp(fitted(gravity))
hist(abs(regression$weight - regression$predictions), breaks = 50)

ggplot(regression %>% 
         filter(weight < 200), aes(x = weight, y = predictions)) +
  geom_hex(bins = 100) + 
  geom_abline(colour = '#848484', size = 1, linetype = 2) +
  scale_fill_gradientn(colours = pal, guide = guide_continuous) +
  theme_ver() +
  labs(title =  glue("MAE: {round(mae(regression$weight, regression$predictions), 4)} | SDAE: {round(sdae(regression$weight, regression$predictions), 4)} | MAPE: {round(mape(regression$weight, regression$predictions), 4)} | SDAPE: {round(sdape(regression$weight, regression$predictions), 4)}"),
       caption = glue("mean observed and mean predicted, {round(mean(regression$weight), 2)} and {round(mean(regression$predictions), 2)}")) +
  theme(legend.position = 'bottom') +
  ggsave("observedxpredicted_6.png", height = 8, width = 8, dpi = 300)

error_lines <- 
  regression %>% 
  left_join(distances) %>% 
  left_join(lines) %>% 
  st_as_sf() %>% 
  mutate(percent_error = abs(weight - predictions) / weight,
         absolute_error = abs(weight - predictions),
         error = weight - predictions,
         quartile = ntile(error, 4))

flows <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = error_lines, 
          aes(colour = factor(ntile(error, 9)), lwd = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_lines$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  scale_alpha(range = c(0.1, 0.5), guide = 'none') +
  facet_wrap(~ quartile, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(flows, filename = "absolute_flows_6.png", height = 6, width = 8, dpi = 300)

error_blocks <-
  error_lines %>%
  st_drop_geometry() %>% 
  group_by(focal) %>% 
  summarise(percent_error = mean(percent_error),
            absolute_error = mean(absolute_error),
            error = mean(error)) %>%
  mutate(quartile_error = ntile(error, 4),
         quartile_absolute = ntile(absolute_error, 4), 
         GEOID = focal) %>%
  left_join(shape) %>%
  st_as_sf()

fills <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(absolute_error, 9)), size = absolute_error, alpha = absolute_error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$absolute_error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "absolute error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_absolute, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills, filename = "absolute_blocks_6.png", height = 6, width = 8, dpi = 300)

fills_error <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_point(data = error_blocks %>% 
               st_centroid() %>%
               st_coordinates() %>%
               as_tibble() %>% 
               bind_cols(error_blocks) %>%
               select(-geometry), 
             aes(X, Y, colour = factor(ntile(error, 9)), size = error, alpha = error)) +
  scale_colour_manual(values = pal,
                      labels = as.character(round(quantile(error_blocks$error,
                                                           c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                                           na.rm = TRUE), 2)),
                      name = "error",
                      guide = guide_discrete) +
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.1, 0.9), guide = 'none') +
  facet_wrap(~ quartile_error, ncol = 2) +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(fills_error, filename = "error_blocks_6.png", height = 6, width = 8, dpi = 300)

## looking by decile
maes <- 
  regression %>% 
  select(weight, lean:doubly) %>%
  pivot_longer(cols = lean:doubly) %>% 
  mutate(name = fct_relevel(name, levels = c("lean", "singly", "doubly"))) %>%
  mutate(error = weight - value) %>% 
  group_by(name) %>% 
  mutate(quintile = ntile(weight, 5)) %>% 
  ungroup() %>%
  group_by(quintile, name) %>%
  group_split() %>%
  map_df(function(x){ 
    x %>%  
      mutate(mae = mae(x$weight, x$value))}) %>%
  group_by(name, quintile) %>%
  summarise(mae = mean(mae))

## plotting by decile
regression %>% 
  select(weight, lean:doubly) %>%
  pivot_longer(cols = lean:doubly) %>% 
  mutate(error = weight - value) %>% 
  group_by(name) %>% 
  mutate(quintile = ntile(weight, 5)) %>% 
  ungroup() %>%
  group_by(quintile, name) %>% 
  summarise(observed = mean(weight),
            predicted = mean(value)) %>%
  ungroup() %>%
  pivot_longer(cols = observed:predicted, names_to = "variable") %>%
  mutate(name = fct_relevel(name, levels = c("lean", "singly", "doubly"))) %>%
  left_join(maes) %>%
  ggplot(aes(quintile, value, shape = variable)) +
  geom_point(size = 2) + 
  geom_path(aes(group = quintile), colour = "black") +
  geom_text(aes(x = quintile - 0.25, y = 30, label = round(mae, 2), colour = ntile(mae, 5)), angle = 90, fontface = 'bold') +
  scale_shape_manual(values = c(2, 17)) +
  scale_x_continuous(limits = c(0.5, 5), breaks = 1:5) + 
  scale_colour_gradientn(colours = rev(pal), guide = 'none') +
  facet_wrap(~ name, nrow = 1) + 
  labs(title = "Predicted and Observed Visits by Observed Decile") +
  ylab("") + 
  xlab("") +
  theme_hor() + 
  ggsave("errorxdecile.png", height = 5, width = 8, dpi = 300)

## Adding zeros
od_list <-
  vroom("~/Desktop/R/git/philamonitor/data/processed/od_monthly.csv") %>% 
  left_join(pois) %>%
  filter(cbg %in% shape$GEOID) %>%
  group_by(poi_cbg, cbg) %>%
  summarise(n = n()) %>% 
  select(-n)

total <- 
  od_list %>% 
  rename(focal = poi_cbg,
         target = cbg) %>%
  left_join(edges_grocery) %>% 
  replace_na(list(weight = 0)) %>%
  filter(focal %in% nodes$cbg)

total_lines <- stplanr::od2line(total, 
                                nodes %>% 
                                  st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
                                  st_transform(4269)) %>% 
  mutate(distance = units::drop_units(st_length(geometry)))

ggplot(total_lines,
       aes(x = distance, y = weight)) +
  geom_hex(bins = 100) + 
  scale_fill_gradientn(colours = pal, guide = guide_continuous) +
  theme_ver() +
  theme(legend.position = 'bottom') + 
  ggsave("weightxdistance.png", height = 6, width = 8, dpi = 300)

## context
poi_map <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(data = pois %>% 
            filter(str_detect(str_to_lower(sub_category), "supermarket")) %>% 
            left_join(footprints) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
          aes(colour = factor(ntile(area_square_feet, 9)),size = area_square_feet, alpha = area_square_feet), show.legend = FALSE) +
  scale_colour_manual(values = pal) + 
  scale_size_continuous(range = c(0.5, 5), guide = 'none') +
  scale_alpha(range = c(0.5, 1), guide = 'none') +
  theme_map() +
  theme(legend.position = 'bottom')

ggsave(poi_map, filename = "pois.png", height = 6, width = 8, dpi = 300)

