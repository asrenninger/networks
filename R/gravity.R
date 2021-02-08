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
temp <- filter(regression, month == 4)

gravity <- 
  glm(log(weight) ~
        log(distance) +  population +
        log(median_income) +  log(businesses + 1) + college_degree + household_size + 
        log(D_j) + log(O_i), family = poisson(link = "log"), 
      data = temp)

temp$predictions <- exp(fitted(gravity))
rsquared(temp$weight, temp$predictions)

summary(gravity)

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
edges <- map_df(index, function(x) { get_bipartite(codes, x, nodes$cbg) %>% mutate(month = as.numeric(x)) })

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
            aes(x = longitude, y = latitude, group = GEOID, colour = log(visits), alpha = log(visits)), arrow = grid::arrow(length = unit(0.05, unit = "inches"), type = 'closed')) +
  geom_sf(data = tract, 
          aes(), fill = NA, colour = '#ffffff', lwd = 0.05, alpha = 0.01) + 
  scale_colour_scico(palette = 'tokyo', guide = 'none') +
  scale_alpha_continuous(range = c(0.5, 1), guide = 'none') +
  facet_wrap(~ lubridate::month(month, label = TRUE, abbr = FALSE), ncol = 4) + 
  coord_sf(crs = 4326) + 
  labs(title = "Interaction Winds") +
  theme_black() +
  ggsave("winds.png", height = 15, width = 18, dpi = 300)

####################################
## ICDR flows
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
    mutate(threshold = paste(x))
})

graph <- graph_from_data_frame(graph_df)

ggraph(graph, 'circle') + 
  geom_edge_link(aes(alpha = log(weight)), colour = '#ffffff', show.legend = FALSE) + 
  geom_node_point(colour = '#ffffff', size = 0.5) +
  scale_alpha_continuous(range = c(0.25, 0.75), guide = 'none') + 
  facet_grid(threshold ~ lubridate::month(month, label= TRUE, abbr = FALSE)) + 
  coord_fixed() +
  theme_black() + 
  ggsave("thresholds_monthxthreshold.png", height = 5, width = 14, dpi = 300)

graph_df <-
  edges %>% 
  filter(poi_cbg != home_cbg) %>%
  group_by(home_cbg, poi_cbg, month) %>%
  summarise(weight = sum(visits)) %>%
  filter(weight > 50) %>% 
  rename(from = home_cbg,
         to = poi_cbg)

graph <- graph_from_data_frame(graph_df)

ggraph(graph, 'circle') + 
  geom_edge_link(aes(alpha = log(weight)), colour = '#ffffff', show.legend = FALSE) + 
  geom_node_point(colour = '#ffffff', size = 0.5) +
  scale_alpha_continuous(range = c(0.25, 0.75), guide = 'none') + 
  facet_wrap(~ lubridate::month(month, label= TRUE, abbr = FALSE)) + 
  coord_fixed() +
  theme_black() + 
  ggsave("thresholds_month.png", height = 6, width = 7.4, dpi = 300)
