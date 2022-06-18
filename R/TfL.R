## TfL API experiments
library(tidyverse)
library(httr)
library(sf)

# base
base <- "https://api.tfl.gov.uk"

# query
modes <- "/Road/All/Status"
dates <- "?startDate=2022-04-01&endDate=2022-04-30"
types <- "&format=json"
query <- paste0(base, modes, dates)

disruptions <-  
  httr::GET(query) %>%
  magrittr::use_series("content") %>% 
  rawToChar() %>%
  jsonlite::fromJSON() %>%
  glimpse() %>%
  mutate(envelope = purrr::map(envelope, ~jsonlite::fromJSON(.x)),
       envelope = purrr::map(envelope, ~st_polygonize(st_linestring(.x)))) %>%
  rename(geometry = envelope) %>%
  st_as_sf(crs = 4326)

# see if it worked
mapview::mapview(disruptions, zcol = "statusSeverity")
