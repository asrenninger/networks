####################################
## Philadelphia, simplex
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")

####################################
## Base data
####################################

## get nodes
codes <- "'42101'"
nodes <- get_nodes(codes)

## get pois
pois <- get_pois(codes, "01")


## get edges
index <- str_pad(1:12, side = 'left', width = 2, pad = "0")
edges <- map_df(index, function(x) { get_bipartite(codes, x, nodes$cbg, 0) %>% mutate(month = as.numeric(x)) })


## getting context data
shape <- 
  reduce(
    map(codes %>% 
          stringr::str_remove_all("\'") %>% 
          stringr::str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          tigris::block_groups(state = stringr::str_sub(x, 1, 2), county = stringr::str_sub(x, 3, 5), class = 'sf')
        }),
    rbind)

water <- 
  reduce(
    map(codes %>% 
          stringr::str_remove_all("\'") %>% 
          stringr::str_split(", ") %>% 
          magrittr::extract2(1),
        function(x) { 
          tigris::area_water(state = stringr::str_sub(x, 1, 2), county = stringr::str_sub(x, 3, 5), class = 'sf')
        }),
    rbind)

## creating background
water <- 
  water %>% 
  sf::st_union() %>%
  sf::st_combine()

background <-
  shape %>% 
  sf::st_union() %>% 
  sf::st_combine() %>% 
  sf::st_difference(water) %>% 
  rmapshaper::ms_simplify(0.05)

####################################
## Features
####################################

## social features
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

## spatial features
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
  filter(safegraph_place_id %in% edges$poi_id) %>%
  mutate(area_square_feet = case_when(str_detect(location_name, "Tubby Olive|Iovine Brothers|Condiment|Giunta's Prime Shop|Olympia Gyro") ~ (area_square_feet / 10) / 5,
                                      TRUE ~ area_square_feet))

unique_links <-
  edges_grocery %>% 
  group_by(focal) %>% 
  summarise(n_unique = n())

businesses <- 
  pois %>% 
  mutate(is_brand = case_when(brand == "NA" ~ FALSE,
                              TRUE ~ TRUE)) %>% 
  left_join(footprints) %>%
  group_by(poi_cbg) %>%
  summarise(businesses = n(), 
            floor_area = sum(area_square_feet, na.rm = TRUE),
            brands = sum(is_brand)) %>%
  rename(focal = poi_cbg) 

## getting businesses 
grocers <- 
  pois %>% 
  filter(str_detect(sub_category, "Supermarket")) %>%
  mutate(is_acme = case_when(brand == "Acme Markets" ~ 1,
                             TRUE ~ 0),
         is_shoprite = case_when(brand == "ShopRite" ~ 1,
                                 TRUE ~ 0),
         is_savealot = case_when(brand == "Save-A-Lot" ~ 1,
                                 TRUE ~ 0),
         is_freshgrocer = case_when(brand == "The Fresh Grocer" ~ 1,
                                    TRUE ~ 0)) %>% 
  left_join(footprints) %>%
  group_by(poi_cbg) %>%
  summarise(grocers = n(), 
            grocers_area = sum(area_square_feet, na.rm = TRUE),
            acmes = sum(is_acme),
            shoprites = sum(is_shoprite),
            savealots = sum(is_savealot),
            freshgrocers = sum(is_freshgrocer)) %>%
  ungroup() %>%
  mutate(important_brands = acmes + shoprites + savealots + freshgrocers) %>%
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
  left_join(unique_links) %>%
  replace_na(list(businesses = 0, grocers = 0)) %>%
  as_tibble() %>% 
  drop_na()

####################################
## Models
####################################

install.packages("tidymodels")
library(tidymodels)

## start here

regression <- readr::read_csv("data/processed/regression.csv")

##  select variables 
vars <- c("distance", "population", "grocers", "grocers_area", "O_i", "D_j")

set.seed(42)
split <- 
  regression %>% 
  select(weight, all_of(vars)) %>%
  initial_split()

train <- training(split)
test <- testing(split)

gravity_r <- 
  recipe(weight ~ ., data = train) %>%
  step_scale(distance, population, grocers_area, O_i, D_j)

boosters <- boost_tree(trees = 1000, 
                        tree_depth = tune(),
                        min_n = tune(),
                        loss_reduction = tune(), 
                        sample_size = tune(),
                        mtry = tune(),
                        learn_rate = tune()) %>%
  set_mode("regression") %>% 
  set_engine("xgboost")

hypercube <- 
  grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size = 40
)

boosting <- 
  workflow() %>% 
  add_formula(weight ~ .) %>%
  add_model(boosters)

gravity_p <- prep(gravity_r)
gravity_j <- juice(gravity_p)

model <- 
  rand_forest(trees = 1000,
              mtry = tune(),
              min_n = tune()) %>%
  set_mode("regression") %>% 
  set_engine("ranger")

tuning <- 
  workflow() %>% 
  add_recipe(gravity_r) %>% 
  add_model(model)

set.seed(24)
folds <- vfold_cv(train)

doParallel::registerDoParallel(cores = 6)

tuned <- 
  tune_grid(
    tuning, 
    resamples = folds,
    grid = 20, 
  )

tuned %>% collect_metrics()

tuned %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")

gridded <- grid_regular(
  mtry(range = c(2, 10)),
  min_n(range = c(2, 10)),
  levels = 5
)

set.seed(42)
regular <- 
  tune_grid(
  tuning,
  resamples = folds,
  grid = gridded
)

regular %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  scale_colour_manual(values = sample(pal, 6)) +
  labs(y = "RMSE") +
  theme_hor() +
  theme(legend.position = 'bottom') +
  ggsave("tuning.png", height = 6, width = 8, dpi = 300)

best <- select_best(regular, "rmse")

final <- finalize_model(
  model,
  best
)

final

library(vip)

theme_set(theme_ver())

final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(weight ~ .,
      data = gravity_j
  ) %>%
  vip(geom = "point") +
  ggsave("vip.png", height = 6, width = 6, dpi = 300)

flow <- 
  workflow() %>%
  add_recipe(gravity_r) %>%
  add_model(final)

results <- 
  flow %>%
  last_fit(split)

results %>%
  collect_metrics()

results %>%
  collect_predictions() %>%
  mutate(error = abs(weight - .pred)) %>%
  summarise(MAE = mean(error))

results %>%
  collect_predictions() %>%
  mutate(error = abs(weight - .pred))

centroids <- 
  regression %>%
  mutate(GEOID = as.character(focal)) %>%
  left_join(shape) %>%
  sf::st_as_sf() %>% 
  sf::st_centroid()

results %>%
  collect_predictions() %>%
  mutate(error = abs(weight - .pred),
         observation = weight) %>%
  bind_cols(test) %>% 
  transmute(prediction = .pred,
            observation,
            error, 
            population, distance, O_i, D_j) %>%
  left_join(centroids) %>% 
  sf::st_as_sf() %>% 
  group_by(GEOID) %>%
  summarise(MAE = mean(error)) %>% 
  ggplot() +
  geom_sf(data = background,
          aes(), fill = NA, colour = '#000000') + 
  geom_sf(aes(colour = MAE, size = log(MAE)), alpha = 0.5) +
  scico::scale_colour_scico(palette = 'hawaii', direction = -1, guide = guide_continuous) +
  scale_size_continuous(rang = c(0.1, 2), guide = 'none') + 
  labs(title = "Random Forest Absolute Error") + 
  theme_map() +
  theme(legend.position = 'bottom') +
  ggsave("foresterrormap.png", height = 8, width = 6, dpi = 300)

results %>%
  collect_predictions() %>%
  mutate(error = abs(weight - .pred)) %>%
  mutate(quantile = ntile(weight, 3)) %>% 
  group_by(quantile) %>% 
  summarise(observed = mean(weight),
            predicted = mean(.pred),
            error = mean(error)) %>%
  pivot_longer(observed:predicted, names_to = "variable", values_to = "value") %>% 
  ungroup() %>%
  ggplot(aes(quantile, value, shape = variable)) +
  geom_point(size = 2) + 
  geom_path(aes(group = quantile), colour = "black") +
  geom_text(aes(x = quantile - 0.25, y = 20, label = round(error, 2), colour = ntile(error, 5)), angle = 90, fontface = 'bold') +
  scale_shape_manual(values = c(2, 17)) +
  scale_x_continuous(limits = c(0.5, 3), breaks = 1:5) + 
  scale_colour_gradientn(colours = rev(pal), guide = 'none') +
  labs(title = "Predicted and Observed Visits by Observed Quantile") +
  ylab("") + 
  xlab("") +
  theme_hor() + 
  ggsave("errorxquantileforest.png", height = 5, width = 8, dpi = 300)

## xgboost comparison
set.seed(42)
boosted <- 
  tune_grid(
  boosting, 
  resamples = folds,
  hypercube,
  control = control_grid(save_pred = TRUE)
)


better <- select_best(boosted, 'rmse')

finished <- 
  finalize_workflow(
    boosting,
    better
  )

finished %>%
  fit(data = train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point") +
  ggsave("boostvip.png", height = 6, width = 6, dpi = 300)

reresults <- 
  finished %>% 
  last_fit(split)

reresults %>%
  collect_predictions() %>%
  mutate(error = abs(weight - .pred)) %>%
  mutate(quantile = ntile(weight, 3)) %>% 
  group_by(quantile) %>% 
  summarise(observed = mean(weight),
            predicted = mean(.pred),
            error = mean(error)) %>%
  pivot_longer(observed:predicted, names_to = "variable", values_to = "value") %>% 
  ungroup() %>%
  ggplot(aes(quantile, value, shape = variable)) +
  geom_point(size = 2) + 
  geom_path(aes(group = quantile), colour = "black") +
  geom_text(aes(x = quantile - 0.25, y = 20, label = round(error, 2), colour = ntile(error, 5)), angle = 90, fontface = 'bold') +
  scale_shape_manual(values = c(2, 17)) +
  scale_x_continuous(limits = c(0.5, 3), breaks = 1:5) + 
  scale_colour_gradientn(colours = rev(pal), guide = 'none') +
  labs(title = "Predicted and Observed Visits by Observed Quantile") +
  ylab("") + 
  xlab("") +
  theme_hor() + 
  ggsave("errorxquantilexgboost.png", height = 5, width = 8, dpi = 300)

reresults %>%
  collect_predictions() %>%
  mutate(error = abs(weight - .pred),
         observation = weight) %>%
  bind_cols(test) %>% 
  transmute(prediction = .pred,
            observation,
            error, 
            population, distance, O_i, D_j) %>%
  left_join(centroids) %>% 
  sf::st_as_sf() %>% 
  group_by(GEOID) %>%
  summarise(MAE = mean(error)) %>% 
  ggplot() +
  geom_sf(data = background,
          aes(), fill = NA, colour = '#000000') + 
  geom_sf(aes(colour = MAE, size = log(MAE)), alpha = 0.5) +
  scico::scale_colour_scico(palette = 'hawaii', direction = -1, guide = guide_continuous) +
  scale_size_continuous(rang = c(0.1, 2), guide = 'none') + 
  labs(title = "XGBoost Absolute Error") + 
  theme_map() +
  theme(legend.position = 'bottom') +
  ggsave("boosterrormap.png", height = 8, width = 6, dpi = 300)

## regression
comparison <- 
  glm(log(weight) ~
        log(distance) + 
        population + 
        #log(grocers + 1) + 
        log(grocers_area + 1) +
        log(O_i) +
        log(D_j), 
      family = poisson(link = "log"), 
      data = test)

comparison_results <-
  test %>% 
  mutate(pred_base = exp(fitted(comparison)))

comparison_results %>%
  mutate(error = abs(weight - pred_base)) %>%
  mutate(quantile = ntile(weight, 3)) %>% 
  group_by(quantile) %>% 
  summarise(observed = mean(weight),
            predicted = mean(pred_base),
            error = mean(error)) %>%
  pivot_longer(observed:predicted, names_to = "variable", values_to = "value") %>% 
  ungroup() %>%
  ggplot(aes(quantile, value, shape = variable)) +
  geom_point(size = 2) + 
  geom_path(aes(group = quantile), colour = "black") +
  geom_text(aes(x = quantile - 0.25, y = 20, label = round(error, 2), colour = ntile(error, 5)), angle = 90, fontface = 'bold') +
  scale_shape_manual(values = c(2, 17)) +
  scale_x_continuous(limits = c(0.5, 3), breaks = 1:5) + 
  scale_colour_gradientn(colours = rev(pal), guide = 'none') +
  labs(title = "Predicted and Observed Visits by Observed Quantile") +
  ylab("") + 
  xlab("") +
  theme_hor() + 
  ggsave("errorxquantilepoisson.png", height = 5, width = 8, dpi = 300)

results %>%
  collect_predictions() %>%
  mutate(error = abs(weight - .pred),
         baseline = abs(weight -  exp(fitted(comparison))),
         observation = weight) %>%
  bind_cols(test) %>% 
  transmute(prediction = .pred,
            observation,
            error, 
            baseline, 
            population, distance, O_i, D_j) %>%
  left_join(centroids) %>% 
  sf::st_as_sf() %>% 
  group_by(GEOID) %>%
  summarise(MAE = mean(error),
            baseline = mean(baseline)) %>%
  mutate(difference = baseline - MAE) %>% 
  ggplot() +
  geom_sf(data = background,
          aes(), fill = NA, colour = '#000000') + 
  geom_sf(aes(colour = difference, size = abs(difference)), alpha = 0.5) +
  scico::scale_colour_scico(palette = 'hawaii', direction = -1, guide = guide_continuous, name = "Gravity - RF") +
  scale_size_continuous(rang = c(0.1, 2), guide = 'none') + 
  labs(title = "Random Forest Against Simple Gravity Model") + 
  theme_map() +
  theme(legend.position = 'bottom') +
  ggsave("poissonforestdifference.png", height = 8, width = 6, dpi = 300)

## turning the dial
scaled <- 
  recipe(weight ~ ., data = regression) %>%
  step_scale(distance, population, grocers_area, O_i, D_j)

prepared <- prep(scaled)

juiced_meme <- juice(prepared)

new_fit <- 
  final %>%
  set_engine("ranger") %>%
  fit(weight ~ .,
      data = juiced_meme
  )

regression %>% 
  filter(focal == "421010012023") %>% 
  select(grocers_area)

updated <-
  regression %>% 
  mutate(grocers_area = if_else(focal == "421010012023", grocers_area + 30000, grocers_area)) %>%
  as_tibble() %>% 
  drop_na()

scaled <- 
  recipe(weight ~ ., data = updated) %>%
  step_scale(distance, population, grocers_area, O_i, D_j)

prepared <- prep(scaled)

juiced_change <- juice(prepared)

prepared_area <- 
  juiced_change %>% 
  filter(focal == "421010012023") %>%
  pull(grocers_area)

pred_1 <- predict(new_fit, juiced_meme)
pred_2 <- predict(new_fit, 
                  juiced_meme %>%
                    mutate(grocers_area = if_else(focal == 421010012023, prepared_area[1], grocers_area)))

lines <- stplanr::od2line(regression %>% 
                            transmute(focal = as.character(focal),
                                      target = as.character(target),
                                      weight = weight), 
                           shape %>% 
                            st_centroid() %>% 
                            transmute(cbg = GEOID) %>%
                            st_transform(4269))

regression %>% 
  mutate(pred_meme = pred_1$.pred,
         pred_change = pred_2$.pred,
         focal = as.character(focal),
         target = as.character(target)) %>%
  filter(pred_meme != pred_change) %>% 
  left_join(lines) %>%
  st_as_sf() %>% 
  transmute(difference = pred_change - pred_meme) %>%
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(aes(colour = difference, lwd = difference)) +
  scale_colour_gradientn(colours = rev(pal),
                         name = "change in travel",
                         guide = guide_continuous) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  theme_map() +
  theme(legend.position = 'bottom') +
  ggsave(filename = "absolute_change_1_forest.png", height = 6, width = 8, dpi = 300)

updated <-
  regression %>% 
  mutate(grocers_area = if_else(focal == "421010010023", grocers_area + 30000, grocers_area)) %>%
  as_tibble() %>% 
  drop_na()

scaled <- 
  recipe(weight ~ ., data = updated) %>%
  step_scale(distance, population, grocers_area, O_i, D_j)

prepared <- prep(scaled)

juiced_change <- juice(prepared)

regression %>% 
  filter(focal == "421010010023") %>% 
  select(grocers_area)

prepared_area <- 
  juiced_change %>% 
  filter(focal == "421010010023") %>%
  pull(grocers_area)

pred_3 <- predict(new_fit, 
                  juiced_meme %>%
                    mutate(grocers_area = if_else(focal == 421010010023, prepared_area[1], grocers_area)))

regression %>% 
  mutate(pred_meme = pred_1$.pred,
         pred_change = pred_3$.pred,
         focal = as.character(focal),
         target = as.character(target)) %>%
  filter(pred_meme != pred_change) %>% 
  left_join(lines) %>%
  st_as_sf() %>% 
  transmute(difference = pred_change - pred_meme) %>%
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5) +
  geom_sf(aes(colour = difference, lwd = difference)) +
  scale_colour_gradientn(colours = rev(pal),
                         name = "change in travel",
                         guide = guide_continuous) +
  scale_size_continuous(range = c(0.1, 1), guide = 'none') +
  theme_map() +
  theme(legend.position = 'bottom') +
  ggsave(filename = "absolute_change_2_forest.png", height = 6, width = 8, dpi = 300)
