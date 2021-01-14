####################################
## Resetting with new metro list
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")
source("R/measure.R")

## grab largest cities according to the census
city_list <- 
  read_csv("data/metrolist_two.csv") %>%
  drop_na(metro_name) %>%
  filter(rank > 45, rank < 101) %>%
  pull(metro_name) %>%
  unique()

## looping through
metrics <- map_df(city_list, ~get_metrics(.x, 1:12))
