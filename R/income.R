####################################
## Looking at thte role of income
####################################

## packages and functions
source("R/package.R")
source("R/help.R")

## scientific notation
options(scipen = 999)

## import results
files <- dir_ls("data/processed/metros/centralities")
centralities <- map_df(files, ~vroom(.x, col_types = cols(GEOID = col_character())))

files <- dir_ls("data/processed/metros/diversities")
diversities <- map_df(files, vroom)

centralities %>% 
  # group_by(month, city) %>% 
  # summarise(s = sum(prc),
  #           m = mean(prc),
  #           v = var(prc),
  #           d = sd(prc)) %>%
  # arrange(city, month) %>%
  filter(str_detect(city, "New York|Los Angeles|San Francisco|Houston|Boston")) %>%
  mutate(city = str_remove_all(city, ", .*")) %>%
  group_by(city, month) %>% 
  mutate(summary = median(deg)) %>%
  ungroup() %>% 
  ggplot(aes(deg, fill = city)) +
  geom_density() +
  geom_vline(aes(xintercept = summary), size = 1, linetype = 3) +
  scale_fill_manual(values = sample(pal, 5), guide = 'none') + 
  scale_x_log10() + 
  labs(x = "", y = "") + 
  facet_grid(lubridate::month(month, label = TRUE, abbr = FALSE) ~ city) +
  theme_hor() +
  ggsave("degreexcityxmonth.png", height = 11, width = 17, dpi = 300)

## import income data
income <- 
  vroom("data/census/data/cbg_b19.csv") %>% 
  filter(census_block_group %in% centralities$GEOID) %>%
  select(census_block_group, B19301e1, B19301m1) %>% 
  transmute(GEOID = census_block_group,
            median_income = B19301e1)

## nest 
nested <- 
  centralities %>% 
  left_join(income) %>%
  group_by(city, month) %>%
  mutate(degree = mean(deg)) %>% 
  ungroup() %>% 
  group_by(city, month, degree) %>%
  nest() 

## save a model
model <- function(df) {
  lm(log(deg + 1) ~ log(median_income), data = df)
}

## run that model on each city
nested <- 
  nested %>% 
  mutate(model = map(data, model))

## add information on those cities
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

coordinates <-
  cities %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>%
  bind_cols(cities) %>% 
  select(metro_name, X, Y) %>%
  rename(city = metro_name)

## plot coefficients
nested %>% 
  mutate(coefficient = purrr::map_dbl(model, function(x){ x$coefficients[[2]] })) %>%
  left_join(coordinates) %>%
  ggplot(aes(x = lubridate::month(month, label = TRUE), y = reorder(city, Y), fill = coefficient)) +
  geom_tile() +
  scale_fill_scico(palette = 'hawaii', guide = guide_continuous, 
                   limits = c(-1, 1), 
                   oob = scales::squish) + 
  labs(caption = glue::glue("log(median income) on degree centrality, ordered from northernmost to southernmost"),
       x = "",
       y = "") + 
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
        legend.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ggsave("degreexincome.png", height = 11, width = 17, dpi = 300)

## run a new model on change from january to april
change <-
  centralities %>% 
  filter(month == 1 | month == 4) %>%
  select(city, GEOID, month, deg) %>%
  pivot_wider(id_cols = city:GEOID, names_from = month, values_from = deg) %>%
  janitor::clean_names() %>%
  filter(x1 > 10) %>%
  mutate(change = (x4 - x1) / x1) %>% 
  rename(GEOID = geoid) %>%
  left_join(income) %>%
  group_by(city) %>%
  mutate(degree = mean(x1)) %>% 
  ungroup() %>% 
  select(-x1, -x4) %>%
  group_by(city, degree) %>%
  nest() %>% 
  ungroup() %>% 
  mutate(model = purrr::map(data, function(x) { lm(change ~ log(median_income), data = x) })) %>% 
  ungroup() %>%
  mutate(coefficient = purrr::map_dbl(model, function(x){ x$coefficients[[2]] }),
         lower = purrr::map_dbl(model, ~pluck(confint(.x), 2)),
         upper = purrr::map_dbl(model, ~pluck(confint(.x), 4))) %>% 
  mutate(sign = if_else(coefficient > 0, "+", "-")) %>%
  left_join(rename(information, city = metro_name)) %>% 
  arrange(rank) %>%
  mutate(block = case_when(rank < 26 ~ "1 - 25",
                           rank > 25 & rank < 51 ~ "26 - 50",
                           rank > 50 & rank < 75 ~ "51 - 75",
                           TRUE ~ "76 - 100")) %>%
  select(city, degree, rank, block, metro_population, coefficient, lower, upper, sign) 

## plot results
ggplot(change, aes(x = reorder(city, metro_population), y = coefficient, colour = sign)) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  scale_colour_manual(values = c(pal[1], pal[9]), guide = 'none') +
  facet_wrap(~ block, scales = 'free_y') +
  coord_flip() +
  labs(caption = glue::glue("log(median income) on Δ degree centrality (Jan - Apr)"),
       x = "",
       y = "") +
  theme_rot() + 
  ggsave("incomecoefficients.png", height = 11, width = 17, dpi = 300)

ggplot(change, 
       aes(x = metro_population, y = coefficient, colour = sign)) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_point(size = 2) + 
  geom_smooth(colour = '#7c7c7c', method = lm, se = FALSE, formula = y ~ poly(x, 2), linetype = 2) + 
  scale_colour_manual(values = c(pal[1], pal[9]), guide = 'none') +
  scale_x_log10() +
  scale_y_continuous(breaks = c(0, -0.1)) +
  labs(caption = glue::glue("log(median income) on Δ degree centrality (Jan - Apr)"),
       x = "population",
       y = "coefficient") +
  theme_ver() +
  ggsave("populationxcoefficients.png", height = 6, width = 8, dpi = 300)

####################################
## Looking at thte role of income
####################################

## import results
files <- dir_ls("data/processed/counties/centralities")
centralities <- map_df(files, ~vroom(.x, col_types = cols(GEOID = col_character())))

## import income data
income <- 
  vroom("data/census/data/cbg_b19.csv") %>% 
  filter(census_block_group %in% centralities$GEOID) %>%
  select(census_block_group, B19301e1, B19301m1) %>% 
  transmute(GEOID = census_block_group,
            median_income = B19301e1)

## nest 
nested <- 
  centralities %>% 
  left_join(income) %>%
  group_by(city, month) %>%
  mutate(degree = mean(deg)) %>% 
  ungroup() %>% 
  group_by(city, month, degree) %>%
  nest() 

## save a model
model <- function(df) {
  lm(log(deg + 1) ~ log(median_income), data = df)
}

## run that model on each city
nested <- 
  nested %>% 
  mutate(model = map(data, model))

## add information on those cities
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

coordinates <-
  cities %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>%
  bind_cols(cities) %>% 
  select(metro_name, X, Y) %>%
  rename(city = metro_name)

## plot coefficients
nested %>% 
  mutate(coefficient = purrr::map_dbl(model, function(x){ x$coefficients[[2]] })) %>%
  left_join(coordinates) %>%
  ggplot(aes(x = lubridate::month(month, label = TRUE), y = reorder(city, Y), fill = coefficient)) +
  geom_tile() +
  scale_fill_scico(palette = 'hawaii', guide = guide_continuous, 
                   limits = c(-1, 1), 
                   oob = scales::squish) + 
  labs(caption = glue::glue("log(median income) on degree centrality, ordered from northernmost to southernmost"),
       x = "",
       y = "") + 
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
        legend.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ggsave("degreexincome.png", height = 11, width = 17, dpi = 300)

## run a new model on change from january to april
change <-
  centralities %>% 
  filter(month == 1 | month == 4) %>%
  select(city, GEOID, month, deg) %>%
  pivot_wider(id_cols = city:GEOID, names_from = month, values_from = deg) %>%
  janitor::clean_names() %>%
  filter(x1 > 10) %>%
  mutate(change = (x4 - x1) / x1) %>% 
  rename(GEOID = geoid) %>%
  left_join(income) %>%
  group_by(city) %>%
  mutate(degree = mean(x1)) %>% 
  ungroup() %>% 
  select(-x1, -x4) %>%
  group_by(city, degree) %>%
  nest() %>% 
  ungroup() %>% 
  mutate(model = purrr::map(data, function(x) { lm(change ~ log(median_income), data = x) })) %>% 
  ungroup() %>%
  mutate(coefficient = purrr::map_dbl(model, function(x){ x$coefficients[[2]] }),
         lower = purrr::map_dbl(model, ~pluck(confint(.x), 2)),
         upper = purrr::map_dbl(model, ~pluck(confint(.x), 4))) %>% 
  mutate(sign = if_else(coefficient > 0, "+", "-")) %>%
  left_join(rename(information, city = metro_name)) %>% 
  arrange(rank) %>%
  mutate(block = case_when(rank < 26 ~ "1 - 25",
                           rank > 25 & rank < 51 ~ "26 - 50",
                           rank > 50 & rank < 75 ~ "51 - 75",
                           TRUE ~ "76 - 100")) %>%
  select(city, degree, rank, block, metro_population, coefficient, lower, upper, sign) 

## plot results
ggplot(change, aes(x = reorder(city, metro_population), y = coefficient, colour = sign)) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  scale_colour_manual(values = c(pal[1], pal[9]), guide = 'none') +
  facet_wrap(~ block, scales = 'free_y') +
  coord_flip() +
  labs(caption = glue::glue("log(median income) on Δ degree centrality (Jan - Apr)"),
       x = "",
       y = "") +
  theme_rot() + 
  ggsave("incomecoefficients.png", height = 11, width = 17, dpi = 300)

ggplot(change, 
       aes(x = metro_population, y = coefficient, colour = sign)) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_point(size = 2) + 
  geom_smooth(colour = '#7c7c7c', method = lm, se = FALSE, formula = y ~ poly(x, 2), linetype = 2) + 
  scale_colour_manual(values = c(pal[1], pal[9]), guide = 'none') +
  scale_x_log10() +
  scale_y_continuous(breaks = c(0, -0.1)) +
  labs(caption = glue::glue("log(median income) on Δ degree centrality (Jan - Apr)"),
       x = "population",
       y = "coefficient") +
  theme_ver() +
  ggsave("populationxcoefficients.png", height = 6, width = 8, dpi = 300)
