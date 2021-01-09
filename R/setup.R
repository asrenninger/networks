####################################
## Setting up the loop
####################################

source("R/query.R")
source("R/package.R")
source("R/help.R")
source("R/measure.R")

## using rvest to grab a list of cities from wikipedia
library(rvest)

## from css selectors to a list
wiki <- read_html("https://en.wikipedia.org/wiki/List_of_metropolitan_statistical_areas")
elem <- html_nodes(wiki,"#mw-content-text > div.mw-parser-output > table:nth-child(12)")

info <- 
  html_table(elem, fill = TRUE, trim = TRUE, header = TRUE) %>% 
  magrittr::extract2(1) %>% 
  as_tibble() %>% 
  clean_names()

city_list <- 
  info %>% 
  filter(rank < 21) %>% 
  mutate(city = str_extract(metropolitan_statistical_area, ".*?-")) %>%
  mutate(city = str_remove_all(city, "-| City|, MO")) %>%
  replace_na(list(city = "Detroit")) %>%
  pull(city)

## looping through
metrics <- map_df(city_list, ~get_metrics(.x, 1:12))