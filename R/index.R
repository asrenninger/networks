########################################
## Deprivation & Demography
########################################

source("R/package.R")
source("R/help.R")

##

moves <- vroom("data/processed/moves_monthly.csv")
phila <- read_sf("data/processed/phila.geojson")

##

blocks <- block_groups("PA", "Philadelphia", cb = TRUE, class = 'sf')

##

library(tidycensus)

sf1 <- c(white = "P005003",
         black = "P005004",
         asian = "P005006",
         hispanic = "P004003")

race <- get_decennial(geography = "block group", variables = sf1,
                       state = "PA", county = "Philadelphia County", geometry = TRUE,
                       summary_var = "P001001")

##

race %>%
  mutate(percent = 100 * (value / summary_value)) %>%
  #mutate(variable = str_to_title(variable)) %>%
  ggplot(aes(fill = percent)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 3857) +
  scale_fill_gradientn(colours = rev(pal), 
                       guide = guide_continuous) +
  labs(title = "Philadelphia Demography", subtitle = "Resident mix by block group") +
  theme_map() +
  theme(legend.position = 'bottom') +
  ggsave("demography.png", height = 8, width = 8, dpi = 300)

##

odmat <- vroom("data/processed/od_monthly.csv")

black <- 
  race %>%
  st_drop_geometry() %>%
  filter(variable == "black") %>%
  mutate(value = value / summary_value) %>%
  transmute(cbg = GEOID, pct_black = value)

##

datum <- 
  odmat %>%
  mutate(month = lubridate::month(start, label = TRUE, abbr = FALSE)) %>% 
  left_join(phila) %>% 
  rename(geocode1 = GEOID) %>%
  select(location_name, safegraph_place_id, cbg, visits, month, geocode1) %>%
  rename(GEOID = cbg) %>% 
  left_join(blocks) %>%
  drop_na(ALAND, AWATER) %>% 
  st_as_sf() %>%
  transmute(geocode1 = geocode1,
            geocode2 = GEOID,
            visits = visits,
            month = month) %>%
  st_drop_geometry()

##

lines <- stplanr::od2line(flow = datum, zones = transmute(blocks, geocode = GEOID))

distances <- 
  lines %>%
  st_transform(3857) %>% 
  mutate(distance = st_length(geometry)) %>%
  mutate(percentile = ntile(distance, 100)) %>%
  filter(percentile > 4 & percentile < 96) %>%
  rename(cbg = geocode2) %>%
  st_drop_geometry() %>%
  group_by(month, cbg) %>%
  summarise(distance = mean(distance))

joined <- left_join(distances, black)
  
##

anim <- 
  ggplot(joined, aes(pct_black, as.numeric(distance))) +
  geom_point(colour = pal[7]) + 
  geom_smooth(method = lm, se = FALSE, colour = pal[9]) +
  ylab("mean travel distance by neighborhood") +
  xlab("percent african american") +
  labs(title = "Race-Activity Relationship", subtitle = "{current_frame}") + 
  transition_manual(month) +
  ease_aes() + 
  theme_ver() 

anim_save("race.gif", animation = anim, 
          height = 400, width = 600)

##

tracts <- tracts("PA", "Philadelphia", cb = TRUE, class = 'sf')

##

library(tidycensus)

##

vars <- load_variables(year = 2018, dataset = 'acs5')

vars %>% filter(str_detect(str_to_lower(label), "white alone"))
vars %>% filter(str_detect(str_to_lower(label), "education"))

##

acs <- c(income = "B06011_001",
         white = "B02001_002",
         education = "B06009_005",
         gini = "B19083_001", 
         population = "B01001_001")

demos <- get_acs(geography = 'tract', variables = acs, 
                 state = "PA", county = "Philadelphia County", geometry = TRUE,
                 summary_var = "B01001_001")

demos <- demos %>%
  mutate(estimate = case_when(variable == "white" ~ (1 - (estimate / summary_est)),
                              variable == "education" ~ (estimate / summary_est),
                              TRUE ~ estimate)) %>%
  select(GEOID, variable, estimate) %>%
  #pivot_wider(id_cols = 'GEOID', names_from = 'variable', values_from = 'estimate')
  spread(key = 'variable', value = 'estimate')

##

expectancy <- 
  read_csv("data/demography/life_expectancy.csv") %>%
  clean_names() %>%
  transmute(GEOID = tract_id,
            expectancy = e_0)

##

vroom("data/census/metadata/cbg_field_descriptions.csv") %>%
  filter(str_detect(str_to_lower(field_full_name), "average household size")) %>% 
  pull(field_full_name)

##

income <- 
  vroom("data/census/data/cbg_b19.csv") %>% 
  filter(str_sub(census_block_group, 1, 5) == "42101") %>%
  select(census_block_group, B19301e1, B19301m1) %>% 
  transmute(GEOID = census_block_group,
            median_income = B19301e1)

education <- 
  vroom("data/census/data/cbg_b15.csv") %>% 
  filter(str_sub(census_block_group, 1, 5) == "42101") %>% 
  select(census_block_group, B15003e1, B15003e22, B15003m1, B15003m22) %>%
  transmute(GEOID = census_block_group, 
            college_degree = B15003e22 / B15003e1)

vars <- 
  vroom("data/census/metadata/cbg_field_descriptions.csv") %>%
  filter(str_detect(str_to_lower(field_full_name), "no health insurance coverage")) %>% 
  pull(table_id)

health <- 
  vroom("data/census/data/cbg_b27.csv") %>% 
  filter(str_sub(census_block_group, 1, 5) == "42101") %>%
  select(census_block_group, B27010e1, B27010m1, vars) %>%
  transmute(GEOID = census_block_group, 
            lack_healthcare = (B27010e17 + B27010e33 + B27010e50 + B27010e66) / B27010e1)

size <- 
  vroom("data/census/data/cbg_b25.csv") %>% 
  filter(str_sub(census_block_group, 1, 5) == "42101") %>%
  select(census_block_group, B25010e1) %>%
  transmute(GEOID = census_block_group, 
            household_size = B25010e1)

nonwhite <- 
  race %>%
  st_drop_geometry() %>%
  filter(variable == 'white') %>%
  transmute(GEOID = GEOID,
            nonwhite = 1 - (value / summary_value),
            population = summary_value)

demos <- 
  income %>% 
  left_join(education) %>%
  left_join(health) %>%
  left_join(nonwhite) %>% 
  left_join(size)

