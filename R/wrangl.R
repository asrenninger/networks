####################################
## Setting up the loop
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")

##  getting fips codes
population <- 
  get_acs(geography = "county", variables = "B01001_001") %>%
  transmute(county_fips = GEOID, county_name = NAME, population = estimate)

cities <-  
  core_based_statistical_areas(class = 'sf') %>% 
  transmute(metro_fips = GEOID, 
            metro_name = NAME)

counties <- 
  counties(class = 'sf') %>%
  transmute(county_fips = GEOID)

joined <- 
  counties %>% 
  left_join(population) %>%
  st_join(cities, largest = TRUE) %>%
  transmute(metro_fips, metro_name, county_fips, county_name, population) %>%
  st_drop_geometry() 

ranked <- 
  joined %>%
  group_by(metro_fips, metro_name) %>%
  summarise(metro_population = sum(population, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(rank = rank(-metro_population)) %>%
  arrange(desc(metro_population))

joined %>% 
  left_join(ranked) %>% 
  arrange(desc(metro_population)) %>% 
  rename(county_population = population) %>% 
  write_csv("metro_list_improved.csv")
