####################################
## Resetting with new metro list
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")
source("R/remeasure.R")

## grab largest cities according to the census
county_list <- 
  read_csv("data/countylist.csv") %>%
  drop_na(county_name) %>%
  # filter(rank > 45, rank < 101) %>%
  pull(county_name) %>%
  unique()

## looping through
metrics <- map_df(county_list[1:100], ~get_metrics(.x, 1:12))
